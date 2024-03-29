#include <stack>
#include <typeinfo>
#include <llvm/Module.h>
#include <llvm/Function.h>
#include <llvm/Type.h>
#include <llvm/DerivedTypes.h>
#include <llvm/LLVMContext.h>
#include <llvm/PassManager.h>
#include <llvm/Instructions.h>
#include <llvm/CallingConv.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/Assembly/PrintModulePass.h>
#include <llvm/Support/IRBuilder.h>
//#include <llvm/ModuleProvider.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/Support/raw_ostream.h>

using namespace llvm;

class NBlock;

class CodeGenBlock {
public:
    BasicBlock *block;
    std::map<std::string, Value*> locals;
    std::map<std::string, Value*> allocated;
};

class CodeGenContext {
    std::stack<CodeGenBlock *> blocks;
    Function *mainFunction;
    

public:
    Module *module;
    CodeGenContext() { module = new Module("main", getGlobalContext()); }
    
    void generateCode(NBlock& root);
    GenericValue runCode();
    std::map<std::string, Value*>& locals() { return blocks.top()->locals; }
    std::map<std::string, Value*>& allocated() { return blocks.top()->allocated; }
    BasicBlock *currentBlock() { return blocks.top()->block; }
    void pushBlock(BasicBlock *block) { 
		CodeGenBlock* newCGB = new CodeGenBlock();
		if (!blocks.empty()) newCGB->locals = blocks.top()->locals;
		if (!blocks.empty()) newCGB->allocated= blocks.top()->allocated;
		blocks.push(newCGB); 
		blocks.top()->block = block;  }
    void popBlock() { CodeGenBlock *top = blocks.top(); blocks.pop(); delete top; }
};

static IRBuilder<> Builder(getGlobalContext());
