use crate::{
    codegen::{expr::*, stmt::*, CodeGenContext},
    symbol_resolver::ValueEnum,
    toplevel::{DefinitionId, TopLevelDef},
    typecheck::typedef::{FunSignature, Type},
};
use inkwell::{
    context::Context,
    types::{BasicTypeEnum, IntType},
    values::{BasicValueEnum, PointerValue},
};
use nac3parser::ast::{Expr, Stmt, StrRef};

pub trait CodeGenerator {
    /// Return the module name for the code generator.
    fn get_name(&self) -> &str;

    fn get_size_type<'ctx>(&self, ctx: &'ctx Context) -> IntType<'ctx>;

    /// Generate function call and returns the function return value.
    /// - obj: Optional object for method call.
    /// - fun: Function signature and definition ID.
    /// - params: Function parameters. Note that this does not include the object even if the
    ///   function is a class method.
    fn gen_call<'ctx, 'a>(
        &mut self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        obj: Option<(Type, ValueEnum<'ctx>)>,
        fun: (&FunSignature, DefinitionId),
        params: Vec<(Option<StrRef>, ValueEnum<'ctx>)>,
    ) -> Result<Option<BasicValueEnum<'ctx>>, String>
    where
        Self: Sized,
    {
        gen_call(self, ctx, obj, fun, params)
    }

    /// Generate object constructor and returns the constructed object.
    /// - signature: Function signature of the contructor.
    /// - def: Class definition for the constructor class.
    /// - params: Function parameters.
    fn gen_constructor<'ctx, 'a>(
        &mut self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        signature: &FunSignature,
        def: &TopLevelDef,
        params: Vec<(Option<StrRef>, ValueEnum<'ctx>)>,
    ) -> Result<BasicValueEnum<'ctx>, String>
    where
        Self: Sized,
    {
        gen_constructor(self, ctx, signature, def, params)
    }

    /// Generate a function instance.
    /// - obj: Optional object for method call.
    /// - fun: Function signature, definition ID and the substitution key.
    /// - params: Function parameters. Note that this does not include the object even if the
    ///   function is a class method.
    /// Note that this function should check if the function is generated in another thread (due to
    /// possible race condition), see the default implementation for an example.
    fn gen_func_instance<'ctx, 'a>(
        &mut self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        obj: Option<(Type, ValueEnum<'ctx>)>,
        fun: (&FunSignature, &mut TopLevelDef, String),
        id: usize,
    ) -> Result<String, String> {
        gen_func_instance(ctx, obj, fun, id)
    }

    /// Generate the code for an expression.
    fn gen_expr<'ctx, 'a>(
        &mut self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        expr: &Expr<Option<Type>>,
    ) -> Result<Option<ValueEnum<'ctx>>, String>
    where
        Self: Sized,
    {
        gen_expr(self, ctx, expr)
    }

    /// Allocate memory for a variable and return a pointer pointing to it.
    /// The default implementation places the allocations at the start of the function.
    fn gen_var_alloc<'ctx, 'a>(
        &mut self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        ty: BasicTypeEnum<'ctx>,
    ) -> Result<PointerValue<'ctx>, String> {
        gen_var(ctx, ty)
    }

    /// Return a pointer pointing to the target of the expression.
    fn gen_store_target<'ctx, 'a>(
        &mut self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        pattern: &Expr<Option<Type>>,
    ) -> Result<PointerValue<'ctx>, String>
    where
        Self: Sized,
    {
        gen_store_target(self, ctx, pattern)
    }

    /// Generate code for an assignment expression.
    fn gen_assign<'ctx, 'a>(
        &mut self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        target: &Expr<Option<Type>>,
        value: ValueEnum<'ctx>,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        gen_assign(self, ctx, target, value)
    }

    /// Generate code for a while expression.
    /// Return true if the while loop must early return
    fn gen_while<'ctx, 'a>(
        &mut self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        stmt: &Stmt<Option<Type>>,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        gen_while(self, ctx, stmt)
    }

    /// Generate code for a while expression.
    /// Return true if the while loop must early return
    fn gen_for<'ctx, 'a>(
        &mut self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        stmt: &Stmt<Option<Type>>,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        gen_for(self, ctx, stmt)
    }

    /// Generate code for an if expression.
    /// Return true if the statement must early return
    fn gen_if<'ctx, 'a>(
        &mut self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        stmt: &Stmt<Option<Type>>,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        gen_if(self, ctx, stmt)
    }

    fn gen_with<'ctx, 'a>(
        &mut self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        stmt: &Stmt<Option<Type>>,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        gen_with(self, ctx, stmt)
    }

    /// Generate code for a statement
    /// Return true if the statement must early return
    fn gen_stmt<'ctx, 'a>(
        &mut self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        stmt: &Stmt<Option<Type>>,
    ) -> Result<(), String>
    where
        Self: Sized,
    {
        gen_stmt(self, ctx, stmt)
    }
}

pub struct DefaultCodeGenerator {
    name: String,
    size_t: u32,
}

impl DefaultCodeGenerator {
    pub fn new(name: String, size_t: u32) -> DefaultCodeGenerator {
        assert!(size_t == 32 || size_t == 64);
        DefaultCodeGenerator { name, size_t }
    }
}

impl CodeGenerator for DefaultCodeGenerator {
    fn get_name(&self) -> &str {
        &self.name
    }

    fn get_size_type<'ctx>(&self, ctx: &'ctx Context) -> IntType<'ctx> {
        // it should be unsigned, but we don't really need unsigned and this could save us from
        // having to do a bit cast...
        if self.size_t == 32 {
            ctx.i32_type()
        } else {
            ctx.i64_type()
        }
    }
}
