use std::ptr;
use std::ffi;
use std::collections::HashMap;

use llvm_sys as llvm;
use llvm::prelude::*;

use crate::ast::*;


enum Variable {
    Arg(LLVMValueRef),
    Var(LLVMValueRef),
}

struct Variables {
    vars: HashMap<String, LLVMValueRef>,
}

impl Variables {
    fn new() -> Self {
        Self {
            vars: HashMap::new()
        }
    }

    fn get(&self, name: &str) -> Option<&LLVMValueRef> {
        self.vars.get(name)
    }
    fn get_mut(&mut self, name: &str) -> Option<&mut LLVMValueRef> {
        self.vars.get_mut(name)
    }

    fn push(&mut self, name: String, value: LLVMValueRef) {
        self.vars.insert(name, value);
    }

    fn push_scope(&mut self) {
        // пока контекст глобальный
    }

    fn pop_scope(&mut self) {

    }
}



pub struct Codegen {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    vars: Variables,
}

impl Codegen {
    pub fn new() -> Self {
        unsafe {
            let context = llvm::core::LLVMContextCreate();
            let module = llvm::core::LLVMModuleCreateWithName(b"mod\0".as_ptr() as _);
            let builder = llvm::core::LLVMCreateBuilderInContext(context);
        
            Self {
                context,
                module,
                builder,
                vars: Variables::new()
            }
        }
    }

    pub fn dump_to_file(&self, name: &str) {
        unsafe {
            let name = ffi::CString::new(name).unwrap();
            llvm::core::LLVMPrintModuleToFile(self.module, name.as_ptr() as _, ptr::null_mut());
        }
    }

    pub fn dump(&self) {
        unsafe {
            llvm::core::LLVMDumpModule(self.module);
        }
    }

    pub fn gen(&mut self, program: &Program) {
        unsafe {
            self.add_llvm_extern_func();
        }
        for func in &program.funcs {
            unsafe { self.cg_func(func); }
        }
    }

    unsafe fn get_llvm_type(&mut self, _ty: &Type) -> LLVMTypeRef {
        //let ret_type = llvm::core::LLVMGetTypeByName(M: LLVMModuleRef, Name: *const ::libc::c_char);
        llvm::core::LLVMInt32TypeInContext(self.context)
    }

    unsafe fn dump_llvm_value(&self, val: LLVMValueRef) {
        llvm::core::LLVMDumpValue(val);
        println!();
    }

    unsafe fn add_llvm_extern_func(&self) {
        let func_name = ffi::CString::new(&b"putchar"[..]).unwrap();

        let ret_type = llvm::core::LLVMInt32TypeInContext(self.context);
        let args_types = vec![
            llvm::core::LLVMInt32TypeInContext(self.context),
        ];
        
        let func_type = llvm::core::LLVMFunctionType(ret_type, args_types.as_ptr() as _, args_types.len() as _, 0);

        let func = llvm::core::LLVMAddFunction(self.module, func_name.as_ptr(), func_type);
        llvm::core::LLVMSetFunctionCallConv(func, llvm::LLVMCallConv::LLVMCCallConv as _);
    }

    unsafe fn cg_func(&mut self, function: &Func) {
        let ret_type = self.get_llvm_type(&function.ret_type);
        let args_types: Vec<_> = function.args
            .iter()
            .map(|arg| self.get_llvm_type(&arg.ty))
            .collect();

        let func_type = llvm::core::LLVMFunctionType(ret_type, args_types.as_ptr() as _, args_types.len() as _, 0);
        let func_name = ffi::CString::new(function.ident.as_bytes()).unwrap();
        let func = llvm::core::LLVMAddFunction(self.module, func_name.as_ptr(), func_type);

        let start_bb = llvm::core::LLVMAppendBasicBlockInContext(self.context, func, b"entry\0".as_ptr() as _);
        llvm::core::LLVMPositionBuilderAtEnd(self.builder, start_bb);

        self.vars.push_scope();
        for (i, arg) in function.args.iter().enumerate() {
            let cname = ffi::CString::new(arg.ident.as_bytes()).unwrap();
            
            // Не самое лучшее решение, т.к. мы копируем параметры на стек.
            // Вероятно llvm соптимизирует это, но всё же.
            // TODO: различать изменяемые и неизменяемые параметры (let / let mut)
            // и в зависимости от этого выделять память на стеке или нет
            let arg_value = llvm::core::LLVMGetParam(func, i as _);
            let arg_type = llvm::core::LLVMTypeOf(arg_value);
            let new_arg_value = llvm::core::LLVMBuildAlloca(self.builder, arg_type, cname.as_ptr());
            llvm::core::LLVMBuildStore(self.builder, arg_value, new_arg_value);
            self.vars.push(arg.ident.clone(), new_arg_value);
        }
        if let Stmt::Block(ref stmts) = function.body {
            for stmt in stmts {
                println!("{:?}",stmt);
                self.cg_stmt(func, stmt);
            }
        }
        self.vars.pop_scope();

    }

    unsafe fn make_nonzero_cmp(&self, cond: LLVMValueRef) -> LLVMValueRef {
        let int_type = llvm::core::LLVMInt32TypeInContext(self.context);
        let zero = llvm::core::LLVMConstInt(int_type, 0, 0);
        let cond = llvm::core::LLVMBuildICmp(self.builder,
            llvm::LLVMIntPredicate::LLVMIntNE, cond, zero, "is_nonzero\0".as_ptr() as _);
        cond
    }

    unsafe fn cg_stmt(&mut self, func: LLVMValueRef, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(e) => {
                self.cg_expr(func, e);
            },
            Stmt::Block(stmts) => {
                self.vars.push_scope();
                for stmt in stmts {
                    self.cg_stmt(func, stmt);
                }
                self.vars.pop_scope();
            },
            Stmt::VarDecl(decl) => {
                let int_type = llvm::core::LLVMInt32TypeInContext(self.context);
                let name = ffi::CString::new(decl.ident.as_bytes()).unwrap();
                let value = llvm::core::LLVMBuildAlloca(self.builder, int_type, name.as_ptr());
                self.vars.push(decl.ident.clone(), value);
            },
            Stmt::Ret(e) => {
                let e = self.cg_expr(func, e);
                llvm::core::LLVMBuildRet(self.builder, e);
            },
            Stmt::If(cond, then_body, else_body) => {
                let cond = self.cg_expr(func, cond);
                let cond = self.make_nonzero_cmp(cond);
    
                let merge_bb = llvm::core::LLVMAppendBasicBlockInContext(self.context, func, "merge\0".as_ptr() as _);
                let then_bb = llvm::core::LLVMAppendBasicBlockInContext(self.context, func, "then\0".as_ptr() as _);
                let else_bb = if else_body.is_some() {
                    llvm::core::LLVMAppendBasicBlockInContext(self.context, func, "else\0".as_ptr() as _)
                } else {
                    merge_bb
                };
    
                llvm::core::LLVMBuildCondBr(self.builder, cond, then_bb, else_bb);
    
                llvm::core::LLVMPositionBuilderAtEnd(self.builder, then_bb);
                self.cg_stmt(func, then_body);
                llvm::core::LLVMBuildBr(self.builder, merge_bb);
    
                if let Some(else_body) = else_body {
                    llvm::core::LLVMPositionBuilderAtEnd(self.builder, else_bb);
                    self.cg_stmt(func, else_body);
                    llvm::core::LLVMBuildBr(self.builder, merge_bb);
                }
                llvm::core::LLVMPositionBuilderAtEnd(self.builder, merge_bb);
            },
            Stmt::While(cond, body) => {
                let cond_bb = llvm::core::LLVMAppendBasicBlockInContext(self.context, func, b"cond\0".as_ptr() as _);
                let body_bb = llvm::core::LLVMAppendBasicBlockInContext(self.context, func, b"body\0".as_ptr() as _);
                let merge_bb = llvm::core::LLVMAppendBasicBlockInContext(self.context, func, b"merge\0".as_ptr() as _);
                
                llvm::core::LLVMBuildBr(self.builder, cond_bb);
                llvm::core::LLVMPositionBuilderAtEnd(self.builder, cond_bb);
                let cond = self.cg_expr(func, cond);
                let cond = self.make_nonzero_cmp(cond);
                llvm::core::LLVMBuildCondBr(self.builder, cond, body_bb, merge_bb);
                
                llvm::core::LLVMPositionBuilderAtEnd(self.builder, body_bb);
                self.cg_stmt(func, body);
                llvm::core::LLVMBuildBr(self.builder, cond_bb);

                llvm::core::LLVMPositionBuilderAtEnd(self.builder, merge_bb);
            }
        }
    }

    unsafe fn cg_expr(&mut self, func: LLVMValueRef, expr: &Expr) -> LLVMValueRef {
        match expr {
            Expr::Literal(value) => {
                let int_type = llvm::core::LLVMInt32TypeInContext(self.context);
                llvm::core::LLVMConstInt(int_type, *value as u64, 0)
            },
            Expr::Add(lhs, rhs) => {
                let lhs = self.cg_expr(func, &*lhs);
                let rhs = self.cg_expr(func, &*rhs);
    
                llvm::core::LLVMBuildAdd(self.builder, lhs, rhs, b"addtmp\0".as_ptr() as _)
            },
            Expr::Sub(lhs, rhs) => {
                let lhs = self.cg_expr(func, &*lhs);
                let rhs = self.cg_expr(func, &*rhs);
    
                llvm::core::LLVMBuildSub(self.builder, lhs, rhs, b"sub\0".as_ptr() as _)
            },
            Expr::Mul(lhs, rhs) => {
                let lhs = self.cg_expr(func, &*lhs);
                let rhs = self.cg_expr(func, &*rhs);
    
                llvm::core::LLVMBuildMul(self.builder, lhs, rhs, b"mul\0".as_ptr() as _)
            },
            Expr::Div(lhs, rhs) => {
                let lhs = self.cg_expr(func, &*lhs);
                let rhs = self.cg_expr(func, &*rhs);
    
                llvm::core::LLVMBuildUDiv(self.builder, lhs, rhs, b"div\0".as_ptr() as _)
            },
            Expr::Mod(lhs, rhs) => {
                let lhs = self.cg_expr(func, &*lhs);
                let rhs = self.cg_expr(func, &*rhs);

                llvm::core::LLVMBuildURem(self.builder, lhs, rhs, b"rem\0".as_ptr() as _)
            },
            Expr::Ref(name) => {
                let value = *self.vars.get(name)
                    .expect(&format!("Referenced wariable '{}' not exist", name));
                
                let name = ffi::CString::new(name.as_bytes()).unwrap();
                let value = llvm::core::LLVMBuildLoad(self.builder, value, name.as_ptr());
                value
            },
            Expr::Assign(name, expr) => {
                let expr = self.cg_expr(func, expr);
                let var = *self.vars.get(name)
                    .expect(&format!("Referenced wariable '{}' not exist", name));
                llvm::core::LLVMBuildStore(self.builder, expr, var);
                expr
            },
            Expr::Call(name, args) => {
                let name = ffi::CString::new(name.as_bytes()).unwrap();
                let args: Vec<_> = args
                    .iter()
                    .map(|arg| self.cg_expr(func, arg))
                    .collect();
                let func_ref = llvm::core::LLVMGetNamedFunction(self.module, name.as_ptr());
                llvm::core::LLVMBuildCall(self.builder, func_ref, args.as_ptr() as _, args.len() as _, b"call\0".as_ptr() as _)
            }
        }
    }
}

impl Drop for Codegen {
    fn drop(&mut self) {
        unsafe {
            llvm::core::LLVMDisposeBuilder(self.builder);
            llvm::core::LLVMDisposeModule(self.module);
            llvm::core::LLVMContextDispose(self.context);
        }
    }
}