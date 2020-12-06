use std::ptr;
use std::collections::HashMap;

use llvm_sys as llvm;

use crate::ast::*;

struct SymbolTable {
    symbols: HashMap<String, *mut llvm::LLVMValue>
}


unsafe fn codegen(ast: &Stmt) {
    // Set up a context, module and builder in that context.
    let context = llvm::core::LLVMContextCreate();
    let module = llvm::core::LLVMModuleCreateWithName(b"nop\0".as_ptr() as *const _);
    let builder = llvm::core::LLVMCreateBuilderInContext(context);

    // Get the type signature for void nop(void);
    // Then create it in our module.
    //let void = llvm::core::LLVMVoidTypeInContext(context);
    let u32_t = llvm::core::LLVMInt32TypeInContext(context);
    let function_type = llvm::core::LLVMFunctionType(u32_t, ptr::null_mut(), 0, 0);
    let function = llvm::core::LLVMAddFunction(module, b"main\0".as_ptr() as *const _,
                                                function_type);

    // Create a basic block in the function and set our builder to generate
    // code in it.
    let bb = llvm::core::LLVMAppendBasicBlockInContext(context, function,
                                                        b"entry\0".as_ptr() as *const _);
    llvm::core::LLVMPositionBuilderAtEnd(builder, bb);

    let mut symbols = SymbolTable { symbols: HashMap::new() };
    
    // Emit a `ret void` into the function
    //llvm::core::LLVMBuildRetVoid(builder);

    codegen_stmt(context, builder, function, &mut symbols, &ast);

    // Dump the module as IR to stdout.
    //llvm::core::LLVMDumpModule(module);
    llvm::core::LLVMPrintModuleToFile(module, b"out.s\0".as_ptr().cast(), ptr::null_mut());

    // Clean up. Values created in the context mostly get cleaned up there.
    llvm::core::LLVMDisposeBuilder(builder);
    llvm::core::LLVMDisposeModule(module);
    llvm::core::LLVMContextDispose(context);
}

unsafe fn codegen_stmt(context: *mut llvm::LLVMContext,
                        builder: *mut llvm::LLVMBuilder,
                        func: *mut llvm::LLVMValue,
                        symbols: &mut SymbolTable,
                        stmt: &Stmt) {
    match stmt {
        Stmt::Expr(e) => {
            codegen_expr(context, builder, symbols, e);
        },
        Stmt::Block(stmts) => {
            for stmt in stmts {
                codegen_stmt(context, builder, func, symbols, stmt);
            }
        },
        Stmt::VarDecl(decl) => {
            let u32_type = llvm::core::LLVMInt32TypeInContext(context);
            let value = llvm::core::LLVMConstInt(u32_type, 0, 0);
            symbols.symbols.insert(decl.ident.clone(), value);
        },
        Stmt::Ret(e) => {
            let e = codegen_expr(context, builder, symbols, e);
            llvm::core::LLVMBuildRet(builder, e);
        },
        Stmt::If(cond, if_body, else_body) => {
            let cond = codegen_expr(context, builder, symbols, cond);
            let u32_type = llvm::core::LLVMInt32TypeInContext(context);
            let zero = llvm::core::LLVMConstInt(u32_type, 0, 0);
            let cond = llvm::core::LLVMBuildICmp(builder, llvm::LLVMIntPredicate::LLVMIntEQ, cond, zero, "is_nonzero\0".as_ptr() as _);

            let if_bb = llvm::core::LLVMAppendBasicBlockInContext(context, func, "then\0".as_ptr() as _);
            let else_bb = llvm::core::LLVMAppendBasicBlockInContext(context, func, "else\0".as_ptr() as _);
            let merge_bb = llvm::core::LLVMAppendBasicBlockInContext(context, func, "merge\0".as_ptr() as _);

            llvm::core::LLVMBuildCondBr(builder, cond, if_bb, else_bb);

            llvm::core::LLVMPositionBuilderAtEnd(builder, if_bb);
            codegen_stmt(context, builder, func, symbols, if_body);
            llvm::core::LLVMBuildBr(builder, merge_bb);

            if let Some(else_body) = else_body {
                llvm::core::LLVMPositionBuilderAtEnd(builder, else_bb);
                codegen_stmt(context, builder, func, symbols, else_body);
                llvm::core::LLVMBuildBr(builder, merge_bb);
            }
            llvm::core::LLVMPositionBuilderAtEnd(builder, merge_bb);
        },
        Stmt::While(..) => {
            // ..
        }
    }
}


unsafe fn codegen_expr(context: *mut llvm::LLVMContext, builder: *mut llvm::LLVMBuilder, symbols: &mut SymbolTable, expr: &Expr) -> *mut llvm::LLVMValue {
    match expr {
        Expr::Literal(value) => {
            let u32_type = llvm::core::LLVMInt32TypeInContext(context);
            llvm::core::LLVMConstInt(u32_type, *value as u64, 0)
        },
        Expr::Add(lhs, rhs) => {
            let lhs = codegen_expr(context, builder, symbols, &*lhs);
            let rhs = codegen_expr(context, builder, symbols, &*rhs);

            llvm::core::LLVMBuildAdd(builder, lhs, rhs, b"addtmp\0".as_ptr() as _)
        },
        Expr::Sub(lhs, rhs) => {
            let lhs = codegen_expr(context, builder, symbols, &*lhs);
            let rhs = codegen_expr(context, builder, symbols, &*rhs);

            llvm::core::LLVMBuildSub(builder, lhs, rhs, b"subtmp\0".as_ptr() as _)
        },
        Expr::Mul(lhs, rhs) => {
            let lhs = codegen_expr(context, builder, symbols, &*lhs);
            let rhs = codegen_expr(context, builder, symbols, &*rhs);

            llvm::core::LLVMBuildMul(builder, lhs, rhs, b"multmp\0".as_ptr() as _)
        },
        Expr::Div(lhs, rhs) => {
            let lhs = codegen_expr(context, builder, symbols, &*lhs);
            let rhs = codegen_expr(context, builder, symbols, &*rhs);

            llvm::core::LLVMBuildUDiv(builder, lhs, rhs, b"divtmp\0".as_ptr() as _)
        },
        Expr::Ref(name) => {
            let value = *symbols.symbols.get(name).unwrap();
            value
        }
        Expr::Assign(name, e) => {
            let e = codegen_expr(context, builder, symbols, e);
            *symbols.symbols.get_mut(name).unwrap() = e;
            e
        }
    }
}