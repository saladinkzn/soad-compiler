#include <iostream>
#include <vector>
#include <llvm/Value.h>

class CodeGenContext;
class NStatement;
class NExpression;
class NVariableDeclaration;

typedef std::vector<NStatement*> StatementList;
typedef std::vector<NVariableDeclaration*> VariableList;

class Node {
public:
	virtual ~Node() {}
	virtual llvm::Value* codeGen(CodeGenContext& context) { }
};

class NExpression : public Node {
};

class NStatement : public Node {
};

class NInteger : public NExpression {
public:
	long long value;
	NInteger(long long value) : value(value) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};



class NDouble : public NExpression {
public:
	double value;
	NDouble(double value) : value(value) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NString : public NExpression {
public:
	std::string value;
	NString(const std::string& value) : value(value) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};
class NIdentifier : public NExpression {
public:
	std::string name;
	NIdentifier(const std::string& name) : name(name) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NMethodCall : public NExpression {
public:
	const NIdentifier& id;
	NExpression& arguments;
	NMethodCall(const NIdentifier& id, NExpression& arguments) :
		id(id), arguments(arguments) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NReadCall : public NExpression {
public:
	const NIdentifier& id;
	NReadCall (const NIdentifier& id) :
		id(id) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NBinaryOperator : public NExpression {
public:
	int op;
	NExpression& lhs;
	NExpression& rhs;
	NBinaryOperator(NExpression& lhs, int op, NExpression& rhs) :
		lhs(lhs), rhs(rhs), op(op) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NUnaryOperator : public NExpression {
public:
	int op;
	NExpression& lhs;
	NUnaryOperator (NExpression& lhs, int op) :
		lhs(lhs), op(op) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NAssignment : public NExpression {
public:
	NIdentifier& lhs;
	NExpression& rhs;
	NAssignment(NIdentifier& lhs, NExpression& rhs) : 
		lhs(lhs), rhs(rhs) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};



class NArrayItem: public NExpression {
public:
	NIdentifier& id;
	NExpression& index;
	NArrayItem(NIdentifier& id, NExpression& index) : 
		id(id), index(index) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NArrAssignment : public NExpression {
public:
	NArrayItem& lhs;
	NExpression& rhs;
	NArrAssignment (NArrayItem& lhs, NExpression& rhs) : 
		lhs(lhs), rhs(rhs) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NBlock : public NExpression {
public:
	StatementList statements;
	NBlock() { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NIfExpr : public NExpression {
public:
	NExpression& cond;
	NBlock& left;
	NBlock& right;
	NIfExpr (NExpression& cond, NBlock& left, NBlock& right) : 
		cond(cond), left(left), right(right) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NCycleExpr: public NExpression {
public:
	NExpression& cond;
	NBlock& body;
	NCycleExpr(NExpression& cond, NBlock& body) : 
		cond(cond), body(body) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NExpressionStatement : public NStatement {
public:
	NExpression& expression;
	NExpressionStatement(NExpression& expression) : 
		expression(expression) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NVariableDeclaration : public NStatement {
public:
	const NIdentifier& id;
	NIdentifier& type;
	bool isArr;
	NVariableDeclaration(const NIdentifier& id, NIdentifier& type, bool isArr) :
		type(type), id(id), isArr(isArr) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NCast : public NExpression{
public:
	NIdentifier& type;
	NExpression& expression;
	bool isArr;
	NCast (NIdentifier& type, NExpression& expression, bool isArr) :
		type(type), expression(expression), isArr(isArr) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NConstArray : public NExpression {
public:
	std::vector<NExpression*> items;
	NConstArray (std::vector<NExpression*> items) : items(items) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NFunctionDeclaration : public NStatement {
public:
	const NIdentifier& type;
	const NIdentifier& id;
	VariableList arguments;
	NBlock& block;
	NFunctionDeclaration(const NIdentifier& type, const NIdentifier& id, 
			const VariableList& arguments, NBlock& block) :
		type(type), id(id), arguments(arguments), block(block) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};