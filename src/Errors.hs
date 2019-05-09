--
-- ###################
-- Error messages file
-- ###################
--

module Errors where

import PrintGram

--
-- Compile-time errors
--

ceVarNoInit var = "variable not initialized: " ++ var
ceVarReadOnly var = "cannot assign value to read-only variable: " ++ var

ceVarUndeclared var = "variable not declared: " ++ var
ceVarDeclType var = "types mismatch in variable declaration: " ++ var
ceVarRedecl var = "variable redeclaration: " ++ var
ceVarRedeclRead var = "cannot redeclare read-only variable: " ++ var

ceVarGlobalType var = "types mismatch for global variable assignment: " ++ var
ceVarGlobalRedecl var = "global variable redeclaration: " ++ var
ceVarGlobalAss = "expression assigned to global variable is not a constant value"

ceVarVoid = "variable declared with wrong type: void"
ceVarFun = "variable declared with wrong type: fun"

ceFunUndeclared var = "function not declared: " ++ var
ceFunRedecl var = "function redeclaration: " ++ var
ceFunPars fun = "wrong number of arguments in function: " ++ printTree fun
ceFunType fun = "types mismatch in function statement: " ++ printTree fun

ceTypeAss ass = "types mismatch in variable assignment: " ++ printTree ass
ceTypeRet ret = "types mismatch in return: " ++ printTree ret

ceTypeInc id = "incremented variable not an int: " ++ printTree id
ceTypeDec id = "decremented variable not an int: " ++ printTree id
ceTypeFor id = "for loop argument not an int: " ++ printTree id
ceTypeFor1 e = "for loop start expression is not an int: " ++ printTree e
ceTypeFor2 e = "for loop end expression is not an int: " ++ printTree e
ceTypeInt e = "expression not an int: " ++ printTree e
ceTypeNegInt e = "negated expression not an int: " ++ printTree e

ceTypeBool e = "expression not a boolean: " ++ printTree e
ceTypeNotBool e = " \"Not\" expression not a boolean: " ++ printTree e
ceTypeCond e = "condition expression not a boolean: " ++ printTree e
ceTypeWhile e = "while loop condition expression is not a boolean: " ++ printTree e

ceMainNotFound = "main function not found"
ceMainRet = "main function return type should be int"
ceMainArgs = "main function does not take any arguments"

ceNonVoidEnd var = "reached end of non-void function " ++ var
