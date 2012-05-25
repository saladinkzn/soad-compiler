#include "node.h"
#include "codegen.h"
#include "parser.hpp"
#include <string>

using namespace std;

void printDouble(double d) {
	std::cout << d << endl;
}

void printInt(int d) {
	std::cout << d << endl;
}

void printString(char* d) {
	std::cout << d << endl;
}

void printChar(char d) {
	std::cout << d << endl;
}

double readDouble() {
	double d;
	std::cin >> d;
	return d;
}

int readInt() {
	int d;
	std::cin >> d;
	return d;
}

char* concat(char* a, char* b) {
	char* buf = new char[strlen(a)+strlen(b)+1];
	strcpy(buf, a);
	strcat(buf, b);
	return buf;
}

/* Compile the AST into a module */
void CodeGenContext::generateCode(NBlock& root)
{
	std::cout << "Generating code...\n";
	{
	vector<Type*> argType;
	argType.push_back(Type::getDoubleTy(getGlobalContext()));
	FunctionType *tellType = FunctionType::get(Type::getVoidTy(getGlobalContext()), makeArrayRef(argType), false);
	Function* tellDoubleFunction = Function::Create(tellType, GlobalValue::ExternalLinkage, "tellDouble", module);
	}
	//	
	{
	vector<Type*> argType;
	argType.push_back(Type::getInt64Ty(getGlobalContext()));
	FunctionType *tellType = FunctionType::get(Type::getVoidTy(getGlobalContext()), makeArrayRef(argType), false);
	Function* tellIntegerFunction = Function::Create(tellType, GlobalValue::ExternalLinkage, "tellInteger", module);
	}
	//
	{
	vector<Type*> argType;
	argType.push_back(PointerType::get(IntegerType::get(getGlobalContext(), 8), 0));
	FunctionType *tellType = FunctionType::get(Type::getVoidTy(getGlobalContext()), makeArrayRef(argType), false);
	Function* tellStringFunction = Function::Create(tellType, GlobalValue::ExternalLinkage, "tellString", module);
	}
	{
	vector<Type*> argType;
	argType.push_back(IntegerType::get(getGlobalContext(), 8));
	FunctionType *tellType = FunctionType::get(Type::getVoidTy(getGlobalContext()), makeArrayRef(argType), false);
	Function* tellStringFunction = Function::Create(tellType, GlobalValue::ExternalLinkage, "tellChar", module);
	}
	{
	vector<Type*> argType;
	FunctionType *readType = FunctionType::get(Type::getDoubleTy(getGlobalContext()), makeArrayRef(argType), false);
	Function* readDoubleFunction = Function::Create(readType , GlobalValue::ExternalLinkage, "readDouble", module);
	}
	{
	vector<Type*> argType;
	FunctionType *readType = FunctionType::get(Type::getInt64Ty(getGlobalContext()), makeArrayRef(argType), false);
	Function* readIntegerFunction = Function::Create(readType, GlobalValue::ExternalLinkage, "readInteger", module);
	}
	{
	vector<Type*> argType;
	argType.push_back(PointerType::get(IntegerType::get(getGlobalContext(), 8), 0));
	argType.push_back(PointerType::get(IntegerType::get(getGlobalContext(), 8), 0));
	FunctionType *readType = FunctionType::get(PointerType::get(IntegerType::get(getGlobalContext(), 8), 0), makeArrayRef(argType), false);
	Function* concatFunction = Function::Create(readType, GlobalValue::ExternalLinkage, "concat", module);
	}

	/* Create the top level interpreter function to call as entry */
	vector<Type*> argTypes;
	FunctionType *ftype = FunctionType::get(Type::getVoidTy(getGlobalContext()), makeArrayRef(argTypes), false);
	std::cout << "creating main function" << endl;
	mainFunction = Function::Create(ftype, GlobalValue::InternalLinkage, "main", module);
	BasicBlock *bblock = BasicBlock::Create(getGlobalContext(), "entry", mainFunction, 0);
	
	/* Push a new variable/block context */
	pushBlock(bblock);
	root.codeGen(*this); /* emit bytecode for the toplevel block */
	ReturnInst::Create(getGlobalContext(), currentBlock());
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
// 	Defining tell function

	ExecutionEngine *ee = EngineBuilder(module).create();
	{	
	Function* tellDoubleFunction = module->getFunction("tellDouble");
	ee->addGlobalMapping(tellDoubleFunction, (void*)(&printDouble));
	}
	{	
	Function* tellIntegerFunction = module->getFunction("tellInteger");
	ee->addGlobalMapping(tellIntegerFunction , (void*)(&printInt));
	}
	{	
	Function* readIntegerFunction = module->getFunction("tellString");
	ee->addGlobalMapping(readIntegerFunction , (void*)(&printString));
	}
	{	
	Function* readIntegerFunction = module->getFunction("tellChar");
	ee->addGlobalMapping(readIntegerFunction , (void*)(&printChar));
	}
	{	
	Function* readDoubleFunction = module->getFunction("readDouble");
	ee->addGlobalMapping(readDoubleFunction , (void*)(&readDouble));
	}
	{	
	Function* readIntegerFunction = module->getFunction("readInteger");
	ee->addGlobalMapping(readIntegerFunction , (void*)(&readInt));
	}
	{	
	Function* readIntegerFunction = module->getFunction("concat");
	ee->addGlobalMapping(readIntegerFunction , (void*)(&concat));
	}

//
	vector<GenericValue> noargs;
	GenericValue v = ee->runFunction(mainFunction, noargs);
	std::cout << "Code was run.\n";
	return v;
}

/* Returns an LLVM type based on the identifier */
static Type *typeOf(const NIdentifier& type, bool isArr) 
{
	Type* _type = Type::getVoidTy(getGlobalContext());
	if (type.name.compare("int") == 0) {
		_type= Type::getInt64Ty(getGlobalContext());
	}
	else if (type.name.compare("double") == 0) {
		_type = Type::getDoubleTy(getGlobalContext());
	}
	else if (type.name.compare("char") == 0) {
		_type = IntegerType::get(getGlobalContext(), 8);
	}
	if(isArr) {
		_type = PointerType::get(_type, 0);
	}
	return _type;
}

/* -- Code Generation -- */

Value* NInteger::codeGen(CodeGenContext& context)
{
	std::cout << "Creating integer: " << value << endl;
	return ConstantInt::get(Type::getInt64Ty(getGlobalContext()), value, true);
}

Value* NDouble::codeGen(CodeGenContext& context)
{
	std::cout << "Creating string: " << value << endl;
	return ConstantFP::get(Type::getDoubleTy(getGlobalContext()), value);
	
}

Value* NString::codeGen(CodeGenContext& context)
{
	const char* val = value.c_str();
	std::cout << "Creating string: " << val << endl;
	Constant* constArray = ConstantArray::get(getGlobalContext(), val , true);
	ArrayType* arrType = ArrayType::get(IntegerType::get(getGlobalContext(), 8), strlen(val)+1);
	GlobalVariable* gVar = new GlobalVariable(*context.module, arrType, true, GlobalValue::PrivateLinkage, (Constant*)0, "", 0, false, 0);
	std::vector<Constant*> indicies;
	indicies.push_back(ConstantInt::get(Type::getInt64Ty(getGlobalContext()), 0, true));
	indicies.push_back(ConstantInt::get(Type::getInt64Ty(getGlobalContext()), 0, true));
	Constant* _const = ConstantExpr::getGetElementPtr(gVar, indicies);
	gVar->setInitializer(constArray);
	return _const;
}

Value* NCycleExpr::codeGen(CodeGenContext& context) {
	Function *function = context.currentBlock()->getParent();
	BasicBlock *LoopBB = BasicBlock::Create(getGlobalContext(), "loop", function);
	BranchInst::Create(LoopBB, context.currentBlock());
	context.pushBlock(LoopBB);
	Value* condition = cond.codeGen(context);
	if (condition == 0) return 0;
	Value* test;
	std::cout << "Condition type: " << endl;
	condition->getType()->dump();
	std::cout << endl;
	bool isConditionInteger = condition->getType() == Type::getInt64Ty(getGlobalContext());
//
	
	if(isConditionInteger ) {
		std::cout << "test type is integer" << endl;
		test= ConstantInt::get(Type::getInt64Ty(getGlobalContext()), 0, true);
		condition = new ICmpInst(*context.currentBlock(), CmpInst::ICMP_NE, condition, test, "ifcond");
	} else {
		std::cout << "test type is double";
		test = ConstantFP::get(Type::getDoubleTy(getGlobalContext()), 0.0);
		condition = new FCmpInst(*context.currentBlock(), CmpInst::FCMP_OEQ, condition, test, "ifcond");
	}
	std::cout << "condition created: " << endl;

	
	std::cout << "function created: " << endl;

	//BasicBlock *Condition = context.currentBlock();
	BasicBlock *BodyBB = BasicBlock::Create(getGlobalContext(), "body");
	BasicBlock *AfterLoopBB = BasicBlock::Create(getGlobalContext(), "afterloop");

//

	BranchInst::Create(BodyBB , AfterLoopBB, condition, context.currentBlock());
	context.popBlock();
	function->getBasicBlockList().push_back(BodyBB);
	context.pushBlock(BodyBB);
	body.codeGen(context);
	BranchInst::Create(LoopBB, context.currentBlock());
	context.popBlock();
	function->getBasicBlockList().push_back(AfterLoopBB);
	context.pushBlock(AfterLoopBB);
	return Constant::getNullValue(Type::getDoubleTy(getGlobalContext()));
}

Value* NIfExpr::codeGen(CodeGenContext& context) {
	Value* condition = cond.codeGen(context);
	if (condition == 0) return 0;
	Value* test;
	std::cout << "Condition type: " << endl;
	condition->getType()->dump();
	std::cout << endl;
	bool isConditionInteger = condition->getType() == Type::getInt64Ty(getGlobalContext());
//
	if(isConditionInteger ) {
		std::cout << "test type is integer" << endl;
		test= ConstantInt::get(Type::getInt64Ty(getGlobalContext()), 0, true);
		condition = new ICmpInst(*context.currentBlock(), CmpInst::ICMP_NE, condition, test, "ifcond");
	} else {
		std::cout << "test type is double";
		test = ConstantFP::get(Type::getDoubleTy(getGlobalContext()), 0.0);
		condition = new FCmpInst(*context.currentBlock(), CmpInst::FCMP_OEQ, condition, test, "ifcond");
	}
	std::cout << "condition created: " << endl;

	Function *function = context.currentBlock()->getParent();
	std::cout << "function created: " << endl;
//
	BasicBlock *ThenBB = BasicBlock::Create(getGlobalContext(), "then", function );
  	BasicBlock *ElseBB = BasicBlock::Create(getGlobalContext(), "else");
  	BasicBlock *MergeBB = BasicBlock::Create(getGlobalContext(), "ifcont");
	
	std::cout << "blocks created: " << endl;
	BranchInst::Create(ThenBB, ElseBB, condition, context.currentBlock());

	 // Emit then value.
	context.pushBlock(ThenBB);
  
  	Value *ThenV = left.codeGen(context);
  	if (ThenV == 0) return 0;
	
	ThenBB->dump();
  	BranchInst::Create(MergeBB, context.currentBlock());
  	// Codegen of 'Then' can change the current block, update ThenBB for the PHI.
	context.popBlock();
  	ThenBB = context.currentBlock();
	std::cout << "then created: " << endl;

	// Emit else block.
	function->getBasicBlockList().push_back(ElseBB);
	context.pushBlock(ElseBB);
	  
	Value *ElseV = right.codeGen(context);
	if (ElseV == 0) return 0;

	BranchInst::Create(MergeBB, ElseBB);
	// Codegen of 'Else' can change the current block, update ElseBB for the PHI.
	context.popBlock();
	ElseBB = context.currentBlock();

	std::cout << "else created: " << endl;
	
	// Emit merge block.
	function->getBasicBlockList().push_back(MergeBB);
	context.pushBlock(MergeBB);
	return ConstantFP::get(Type::getDoubleTy(getGlobalContext()), 0.0);
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

Value* NReadCall::codeGen(CodeGenContext& context)
{
	Type* type;
	if (context.locals().find(id.name) == context.locals().end()) {
		AllocaInst *alloc = new AllocaInst(Type::getInt64Ty(getGlobalContext()), id.name, context.currentBlock());
		context.locals()[id.name] = alloc;
		type = Type::getInt64Ty(getGlobalContext());
	} else {
		Value* arg = context.locals()[id.name];
		type = arg->getType();
	}
	string readFunc("read");
	if(type == Type::getDoubleTy(getGlobalContext())) {
		readFunc+= "Double";
	} else {
		if(type == Type::getInt64Ty(getGlobalContext())) {
			readFunc+= "Integer";
		} else {
			std::cerr  << "Unknown type!" << endl;
			type->dump();
		}
	}
	Function *function = context.module->getFunction(readFunc);
	if (function == NULL) {
		std::cerr << "no such function " << readFunc << endl;
		return NULL;
	}
	
	std::vector<Value*> args;
	CallInst *call = CallInst::Create(function, makeArrayRef(args), "", context.currentBlock());

	std::cout << "Creating method call: " << id.name << endl;
	return new StoreInst(call, context.locals()[id.name], false, context.currentBlock());
}

Value* NMethodCall::codeGen(CodeGenContext& context)
{
	Value* arg = arguments.codeGen(context);
	Type* type = arg->getType();
	Function *function = context.module->getFunction(id.name.c_str());
	if (function == NULL) {
		std::cerr << "no such function " << id.name << " maybe it's overloaded?" << endl;
		//
		string suffix;
		if(type == Type::getDoubleTy(getGlobalContext())) {
			suffix = "Double";
		} else {
			if(type == Type::getInt64Ty(getGlobalContext())) {
			suffix = "Integer";
			} else {
				if(type == PointerType::get(IntegerType::get(getGlobalContext(), 8), 0)) {
					suffix = "String";
				} else {
					if(type == IntegerType::get(getGlobalContext(), 8)) {
						suffix = "Char";
					} else {

						std::cerr << "Unknown type: ";
						type->dump();
						std::cerr << endl;
					}
				}	
			}
		
		}
		function = context.module->getFunction(id.name.c_str() + suffix);
		if (function == NULL) {
			std::cerr << "no such function " << id.name << suffix << " overloaded search failed";
			return NULL;
		}
	}

	
	std::vector<Value*> args;
	args.push_back(arg);
	CallInst *call = CallInst::Create(function, makeArrayRef(args), "", context.currentBlock());
	std::cout << "Creating method call: " << id.name << endl;
	return call;
}

Value* NBinaryOperator::codeGen(CodeGenContext& context)
{
	std::cout << "Creating binary operation " << op << endl;
	Instruction::BinaryOps instr;
	CmpInst::Predicate pred;
	Type* intType = Type::getInt64Ty(getGlobalContext());
	Type* doubleType = Type::getDoubleTy(getGlobalContext());
	Type* strType = PointerType::get(IntegerType::get(getGlobalContext(), 8), 0);
	//
	Value* l = lhs.codeGen(context);
	Type* l_type = l->getType();
	//
	Value* r = rhs.codeGen(context);
	Type* r_type = r->getType();
	if(l_type != r_type) {
		std::cerr << "Type are not same: l_type = "; l_type->dump(); std::cerr << " ";
		std::cerr << "r_type = "; r_type->dump();
	}
	//
	switch (op) {
		case TPLUS: 	instr = Instruction::Add; goto math;
		case TMINUS: 	instr = Instruction::Sub; goto math;
		case TMUL: 		instr = Instruction::Mul; goto math;
		case TDIV: 		instr = Instruction::SDiv; goto math;
		case TCONCAT: if(l_type == strType) {
					Function* function = context.module->getFunction("concat");
					std::vector<Value*> args;
					args.push_back(l);
					args.push_back(r);
					CallInst *call = CallInst::Create(function, makeArrayRef(args), "", context.currentBlock());
					return call;
					
				} else {
					std::cerr << "argument is not string";
				} 
				
		case TCEQ: {
				std::cout << "In TCGT" << endl;
				if (l_type == intType) {
					pred = CmpInst::ICMP_EQ;
					goto cmp;
				} else {
					pred = CmpInst::FCMP_OEQ;
					goto cmp;
				}
			    }
		case TCNE: {
				std::cout << "In TCGT" << endl;
				if (l_type == intType) {
					pred = CmpInst::ICMP_NE;
					goto cmp;
				} else {
					pred = CmpInst::FCMP_ONE;
					goto cmp;
				}
			    }
		case TCLT: {
				std::cout << "In TCGT" << endl;
				if (l_type == intType) {
					pred = CmpInst::ICMP_SLT;
					goto cmp;
				} else {
					pred = CmpInst::FCMP_OLT;
					goto cmp;
				}
			    }
		case TCLE: {
				std::cout << "In TCGT" << endl;
				if (l_type == intType) {
					pred = CmpInst::ICMP_SLE;
					goto cmp;
				} else {
					pred = CmpInst::FCMP_OLE;
					goto cmp;
				}
			    }
		case TCGT:  {
				std::cout << "In TCGT" << endl;
				if (l_type == intType) {
					pred = CmpInst::ICMP_SGT;
					goto cmp;
				} else {
					pred = CmpInst::FCMP_OGT;
					goto cmp;
				}
			    }
		case TCGE:  {
				std::cout << "In TCGT" << endl;
				if (l_type == intType) {
					pred = CmpInst::ICMP_SGE;
					goto cmp;
				} else {
					pred = CmpInst::FCMP_OGE;
					goto cmp;
				}
			    }	
		}
	return NULL;
cmp:
	if (l_type == intType) {
		ICmpInst* tmp = new ICmpInst(*context.currentBlock(),pred, l,r, "");
		CastInst* tmp2 = new ZExtInst(tmp, intType, "", context.currentBlock());
		return tmp2;
	} else {
		FCmpInst* tmp = new FCmpInst(*context.currentBlock(),pred, l,r, "");
		CastInst* tmp2 = new ZExtInst(tmp, intType, "", context.currentBlock());
		return tmp2;
	}

math:
	return BinaryOperator::Create(instr, lhs.codeGen(context), 
		rhs.codeGen(context), "", context.currentBlock());
}

Value* NUnaryOperator::codeGen(CodeGenContext& context)
{
	std::cout << "Creating binary operation " << op << endl;
	Instruction::BinaryOps instr;
	CmpInst::Predicate pred;
	Type* intType = Type::getInt64Ty(getGlobalContext());
	Type* doubleType = Type::getDoubleTy(getGlobalContext());
	//
	Value* l = lhs.codeGen(context);
	Type* l_type = l->getType();
	//
	Value *r;
	switch (op) {
		case TMINUS: 	instr = Instruction::Mul; r = ConstantInt::get(Type::getInt64Ty(getGlobalContext()), -1, true); goto math;
	}
	return NULL;

math:
	return BinaryOperator::Create(instr, l, r, "", context.currentBlock());
}


Value* NAssignment::codeGen(CodeGenContext& context)
{
// 	Computing expression rhs
	Value* rhs_val = rhs.codeGen(context);
	rhs_val->getType()->dump();
	if (context.locals().find(lhs.name) == context.locals().end()) {
		std::cerr << "undeclared variable " << lhs.name << endl;
//
		return 0;

	}
// 
	std::cout << "Assigning" << endl;
	return new StoreInst(rhs_val, context.locals()[lhs.name], false, context.currentBlock());
}

Value* NVariableDeclaration::codeGen(CodeGenContext& context) {
	if (context.locals().find(id.name) != context.locals().end()) {
		std::cerr << "trying to redeclare existing variable " << id.name << endl;
	} else {
		std::cout << "Creating variable declaration " << " " << id.name << endl;
// 		Allocating variable of rhs type, with name lhs.name
		AllocaInst *alloc = new AllocaInst(typeOf(type, isArr), id.name.c_str(), context.currentBlock());
		context.locals()[id.name] = alloc;
	}
	return ConstantInt::get(Type::getInt64Ty(getGlobalContext()), 0, true);
}

Value* NCast::codeGen(CodeGenContext& context) {
	Type* intType = Type::getInt64Ty(getGlobalContext());
	Type* doubleType = Type::getDoubleTy(getGlobalContext());
	Type* charType = IntegerType::get(getGlobalContext(), 8);
	Type* intPtrType = PointerType::get(intType, 0);
	Type* doublePtrType = PointerType::get(doubleType , 0);
	Type* charPtrType = PointerType::get(charType , 0);
	
	Value* exprValue = expression.codeGen(context);
	Type* exprType = exprValue->getType();
	Type* targetType = typeOf(type, isArr);
	Instruction::CastOps castInstruction;
	bool needCast = true;
	if (exprType == intType) {
		if (targetType == intType) { needCast = false; goto cast; }
		if (targetType == doubleType) { castInstruction = Instruction::SIToFP; goto cast; }
		if (targetType == charType) { castInstruction = Instruction::Trunc; goto cast; }
		if (targetType == intPtrType || targetType == doublePtrType || targetType == charPtrType) { castInstruction = Instruction::IntToPtr; goto cast; }
	} 
	if (exprType == doubleType) {
		if (targetType == intType) castInstruction = Instruction::FPToSI; goto cast;
		if (targetType == doubleType) needCast = false; goto cast;
		if (targetType == charType) castInstruction = Instruction::FPToSI; goto cast;
		if (targetType == intPtrType || targetType == doublePtrType || targetType == charPtrType) { std::cerr << "Invalid cast: double -> pointer"; return 0; }   
	}
	if (exprType == charType) {
		if (targetType == intType) { castInstruction = Instruction::SExt; goto cast; }
		if (targetType == doubleType) { castInstruction = Instruction::SIToFP; goto cast; }
		if (targetType == charType) { needCast = false; goto cast; }
		if (targetType == intPtrType || targetType == doublePtrType || targetType == charPtrType) { castInstruction = Instruction::IntToPtr; goto cast; }
	} 
	if (exprType == intPtrType) {
		if (targetType == intType) { castInstruction = Instruction::PtrToInt; goto cast; }
		if (targetType == doubleType) { std::cerr << "Invalid cast: double -> pointer"; return 0;}
		if (targetType == charType) { castInstruction = Instruction::PtrToInt; goto cast; }
		if (targetType == doublePtrType || targetType == charPtrType) { castInstruction = Instruction::BitCast; goto cast; }
		if (targetType == intPtrType) { needCast = false; goto cast; }
	}
	if (exprType == doublePtrType) {
		if (targetType == intType) { castInstruction = Instruction::PtrToInt; goto cast; }
		if (targetType == doubleType) { std::cerr << "Invalid cast: double -> pointer"; return 0;}
		if (targetType == charType) { castInstruction = Instruction::PtrToInt; goto cast; }
		if (targetType == intPtrType|| targetType == charPtrType) { castInstruction = Instruction::BitCast; goto cast; }
		if (targetType == doublePtrType ) { needCast = false; goto cast; }
	}
	if (exprType == charPtrType ) {
		if (targetType == intType) { castInstruction = Instruction::PtrToInt; goto cast; }
		if (targetType == doubleType) { std::cerr << "Invalid cast: double -> pointer"; return 0;}
		if (targetType == charType) { castInstruction = Instruction::PtrToInt; goto cast; }
		if (targetType == intPtrType|| targetType == doublePtrType ) { castInstruction = Instruction::BitCast; goto cast; }
		if (targetType == charPtrType) { needCast = false; goto cast; }
	}
cast :
	if(needCast) {
		return CastInst::Create(castInstruction, exprValue, targetType, "", context.currentBlock());
	} else {
		return exprValue;
	}
}

Value* NArrAssignment::codeGen(CodeGenContext& context)
{
// 	Computing expression rhs
	Value* rhs_val = rhs.codeGen(context);
	std::cout << "Assigning: ";
	rhs_val->dump();
	std::cout << endl;
	if(llvm::ConstantInt* indexInt = dyn_cast<llvm::ConstantInt>(lhs.index.codeGen(context))) {
	//
		uint64_t _len = indexInt->getZExtValue();
		if(context.locals().find(lhs.id.name) == context.locals().end()) {
			std::cerr << "array " << lhs.id.name << " is not defined\n";
			return NULL;
		}
		if(context.allocated().find(lhs.id.name) == context.allocated().end()) {
			std::cout << "Allocating array "<< lhs.id.name << " length = " << _len <<endl;
			ArrayType* arrType = ArrayType::get(Type::getInt64Ty(getGlobalContext())/*context.locals()[lhs.id.name]->getType()*/, _len+1);
			std::cout << "Array type: ";
			arrType->dump();
			std::cout << endl;
			AllocaInst *alloc = new AllocaInst(arrType, lhs.id.name.c_str(), context.currentBlock());
			std::cout << "Allocated array : " << lhs.id.name.c_str() << endl;
			Value* ptr = context.locals()[lhs.id.name];
			std::cout << "Obtaining pointer " << endl;
			ptr->dump();
			//
			std::vector<Value*> indicies2;
			indicies2.push_back(ConstantInt::get(Type::getInt64Ty(getGlobalContext()), 0, true));
			indicies2.push_back(ConstantInt::get(Type::getInt64Ty(getGlobalContext()), 0, true));
			Instruction* arrayBeginPtr = GetElementPtrInst::CreateInBounds(alloc, indicies2, "", context.currentBlock());
			std::cout << "Obtaining arrBeginPtr " << endl;
			arrayBeginPtr->dump();
			std::cout << endl;
			//
			StoreInst* inst = new StoreInst(arrayBeginPtr, ptr, false, context.currentBlock());
			inst->dump();
			std::cout << "Storing allocated array in pointer" << endl;			
			context.allocated()[lhs.id.name] = alloc;
			std::cout << "Adding " << lhs.id.name << " to allocated arrays" << endl;
		}
		//
		std::cout << "Starting assigining value to " << lhs.id.name << "["<<_len <<"]" << endl;
		context.locals()[lhs.id.name]->dump();
		std::cout << endl;
		LoadInst* arrayPtr = new LoadInst(context.locals()[lhs.id.name], "", false, context.currentBlock());
		arrayPtr->dump();
		std::cout << endl;
		std::vector<Value*> indicies;
		indicies.push_back(ConstantInt::get(Type::getInt64Ty(getGlobalContext()), _len, true));
		Instruction* ptr = GetElementPtrInst::CreateInBounds(arrayPtr, indicies, "", context.currentBlock());
		ptr->dump();

		std::cout << "Storing rhs in ptr" << endl;
		StoreInst* inst = new StoreInst(rhs_val, ptr, false, context.currentBlock());
		return inst;
//		return new StoreInst(rhs_val, context.locals()[lhs.id.name], false, context.currentBlock());
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
		std::cout << "Getting array item on position " << index << " in array with name " << id.name << endl;	
		std::vector<Value*> indicies;
		LoadInst* arrPtr = new LoadInst(context.locals()[id.name], "", false, context.currentBlock());
//		indicies.push_back(ConstantInt::get(Type::getInt64Ty(getGlobalContext()), 0, true));
		indicies.push_back(ConstantInt::get(Type::getInt64Ty(getGlobalContext()), index, true));
		std::cout << "Returning" << endl;
		Instruction* ptr = GetElementPtrInst::CreateInBounds(arrPtr, indicies, "", context.currentBlock());
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