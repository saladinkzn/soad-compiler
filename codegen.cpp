#include "node.h"
#include "codegen.h"
#include "parser.hpp"

using namespace std;

/* Compile the AST into a module */
void CodeGenContext::generateCode(NBlock& root)
{
	std::cout << "Generating code...\n";
	
	/* Create the top level interpreter function to call as entry */
	vector<Type*> argTypes;
	FunctionType *ftype = FunctionType::get(Type::getVoidTy(getGlobalContext()), makeArrayRef(argTypes), false);
	mainFunction = Function::Create(ftype, GlobalValue::InternalLinkage, "main", module);
	BasicBlock *bblock = BasicBlock::Create(getGlobalContext(), "entry", mainFunction, 0);
	
	/* Push a new variable/block context */
	pushBlock(bblock);
	root.codeGen(*this); /* emit bytecode for the toplevel block */
	ReturnInst::Create(getGlobalContext(), bblock);
	popBlock();
	
	/* Print the bytecode in a human-readable format 
	   to see if our program compiled properly
	 */
	std::cout << "Code is generated.\n";
	PassManager pm;
	pm.add(createPrintModulePass(&outs()));
	pm.run(*module);
}

/* Executes the AST by running the main function */
GenericValue CodeGenContext::runCode() {
	std::cout << "Running code...\n";
	ExecutionEngine *ee = EngineBuilder(module).create();
	vector<GenericValue> noargs;
	GenericValue v = ee->runFunction(mainFunction, noargs);
	std::cout << "Code was run.\n";
	return v;
}

/* Returns an LLVM type based on the identifier */
static Type *typeOf(const NIdentifier& type) 
{
	if (type.name.compare("int") == 0) {
		return Type::getInt64Ty(getGlobalContext());
	}
	else if (type.name.compare("double") == 0) {
		return Type::getDoubleTy(getGlobalContext());
	}
	return Type::getVoidTy(getGlobalContext());
}

/* -- Code Generation -- */

Value* NInteger::codeGen(CodeGenContext& context)
{
	std::cout << "Creating integer: " << value << endl;
	return ConstantInt::get(Type::getInt64Ty(getGlobalContext()), value, true);
}

Value* NDouble::codeGen(CodeGenContext& context)
{
	std::cout << "Creating double: " << value << endl;
	return ConstantFP::get(Type::getDoubleTy(getGlobalContext()), value);
}

Value* NIdentifier::codeGen(CodeGenContext& context)
{
	std::cout << "Creating identifier reference: " << name << endl;
	if (context.locals().find(name) == context.locals().end()) {
		std::cerr << "undeclared variable " << name << endl;
		return NULL;
	}
	return new LoadInst(context.locals()[name], "", false, context.currentBlock());
}

Value* NMethodCall::codeGen(CodeGenContext& context)
{
	Function *function = context.module->getFunction(id.name.c_str());
	if (function == NULL) {
		std::cerr << "no such function " << id.name << endl;
		return NULL;
	}
	std::vector<Value*> args;
//	ExpressionList::const_iterator it;
//	for (it = arguments.begin(); it != arguments.end(); it++) {
//		args.push_back((**it).codeGen(context));
//	}
	args.push_back(arguments.codeGen(context));
	CallInst *call = CallInst::Create(function, makeArrayRef(args), "", context.currentBlock());
	std::cout << "Creating method call: " << id.name << endl;
	return call;
}

Value* NBinaryOperator::codeGen(CodeGenContext& context)
{
	std::cout << "Creating binary operation " << op << endl;
	Instruction::BinaryOps instr;
	switch (op) {
		case TPLUS: 	instr = Instruction::Add; goto math;
		case TMINUS: 	instr = Instruction::Sub; goto math;
		case TMUL: 		instr = Instruction::Mul; goto math;
		case TDIV: 		instr = Instruction::SDiv; goto math;
		/*
		TODO:
		case TCEQ:  ==  instr = Instruction::; goto math;
		case TCNE:  != ; goto math;
		case TCLT:  "<"  goto math;
		case TCLE:  "<="  goto math;
		case TCGT:  ">"  goto math;
		case TCGE:  ">="  goto math;
		*/ 
	}

	return NULL;
math:
	return BinaryOperator::Create(instr, lhs.codeGen(context), 
		rhs.codeGen(context), "", context.currentBlock());
}

Value* NAssignment::codeGen(CodeGenContext& context)
{
// 	Computing expression rhs
	Value* rhs_val = rhs.codeGen(context);
//
	std::cout << "Creating variable declaration " << " " << lhs.name << endl;
// 	Allocating variable of rhs type, with name lhs.name
	AllocaInst *alloc = new AllocaInst(rhs_val->getType(), lhs.name.c_str(), context.currentBlock());
	context.locals()[lhs.name] = alloc;
// 
	std::cout << "Assigning" << endl;
	return new StoreInst(rhs_val, context.locals()[lhs.name], false, context.currentBlock());
}

Value* NArrAssignment::codeGen(CodeGenContext& context)
{
// 	Computing expression rhs
	Value* rhs_val = rhs.codeGen(context);
	if(llvm::ConstantInt* indexInt = dyn_cast<llvm::ConstantInt>(lhs.index.codeGen(context))) {
	//
		uint64_t _len = indexInt->getZExtValue();
		std::cout << "Creating array declaration "<< lhs.id.name << " length =" << _len <<endl;
		ArrayType* arrType = ArrayType::get(rhs_val->getType(), _len+1);
		AllocaInst *alloc = new AllocaInst(arrType, lhs.id.name.c_str(), context.currentBlock());
		context.locals()[lhs.id.name] = alloc;
		//
		std::vector<Value*> indicies;
		indicies.push_back(ConstantInt::get(Type::getInt64Ty(getGlobalContext()), 0, true));
		indicies.push_back(ConstantInt::get(Type::getInt64Ty(getGlobalContext()), _len, true));
		Instruction* ptr = GetElementPtrInst::Create(alloc, indicies, "", context.currentBlock());
		StoreInst* inst = new StoreInst(rhs_val, ptr, false, context.currentBlock());
		return new StoreInst(rhs_val, context.locals()[lhs.id.name], false, context.currentBlock());
	} else {
		std::cout << "Index is not an integer" << endl;
		return NULL;
	}
}

Value* NArrayItem::codeGen(CodeGenContext& context)
{
	std::cout << "Creating array item reference: " << id.name << endl;
	if (context.locals().find(id.name) == context.locals().end()) {
		std::cerr << "undeclared variable " << id.name << endl;
		return NULL;
	}
	Value* rhs_val = index.codeGen(context);
	if(llvm::ConstantInt* indexInt = dyn_cast<llvm::ConstantInt>(rhs_val)) {
		uint64_t index = indexInt->getZExtValue();
		std::cout << "Setting array item on position " << index << " in array with name " << id.name << endl;	
		std::vector<Value*> indicies;
		indicies.push_back(ConstantInt::get(Type::getInt64Ty(getGlobalContext()), 0, true));
		indicies.push_back(ConstantInt::get(Type::getInt64Ty(getGlobalContext()), index, true));
		std::cout << "Returning" << endl;
		Instruction* ptr = GetElementPtrInst::Create(context.locals()[id.name], indicies, "", context.currentBlock());
		LoadInst* item = new LoadInst(ptr, "", false, context.currentBlock());
		return item;
	} else {
		std::cout << " Index is not integer ";
		return NULL;
	}

}

Value* NBlock::codeGen(CodeGenContext& context)
{
	StatementList::const_iterator it;
	Value *last = NULL;
	for (it = statements.begin(); it != statements.end(); it++) {
		std::cout << "Generating code for " << typeid(**it).name() << endl;
		last = (**it).codeGen(context);
	}
	std::cout << "Creating block" << endl;
	return last;
}

Value* NExpressionStatement::codeGen(CodeGenContext& context)
{
	std::cout << "Generating code for " << typeid(expression).name() << endl;
	return expression.codeGen(context);
}