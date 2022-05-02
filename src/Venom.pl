venom(Lexername, Filename) :-
    term_to_atom(Y, X),
    program(Tree, Y, []),
    evalProgram(Tree, Output).

:- table boolean/3, expression/3, term/3.

% Parsing the program 
program(program(M)) -->['begin'], block(M), ['end'].

% Parsing the block
block(block(M)) --> ['{'], blockSection(M), ['}']. 
blockSection(block(M)) --> statements(M).
blockSection(block(M, N)) --> statements(M), blockSection(N).

% Parsing statements 
statements(statements(X)) --> 
    declaration(X), [;].
statements(statements(X)) --> 
    assignment(X), [;].
statements(statements(X)) --> 
    expression(X), [;].
statements(statements(X)) --> 
    boolean(X), [;].
statements(statements(X)) --> 
    printstatements(X), [;].
statements(statements(X)) --> 
    if_cond(X).
statements(statements(X)) --> 
    ternarycondition(X), [;].
statements(statements(X)) --> 
    for(X).
statements(statements(X)) --> 
    while(X).
statements(statements(X)) --> 
    for_in_range(X).
statements(statements(X)) --> 
    iterator(X), [;].

%to parse variable declaration
declaration(declare(int, M, N)) --> ['int'], identifier(M), ['='], expression(N).
declaration(declare(string, M, N)) --> ['string'], identifier(M), ['='], string(N).
declaration(declare(boolean, M, true)) --> ['bool'], identifier(M), [=], ['true'].
declaration(declare(boolean, M, false)) --> ['bool'], identifier(M), [=], ['false'].
declaration(declare(M, N)) --> dataType(M), identifier(N).

%to parse datatype
dataType(int) --> ['int'].
dataType(string) --> ['string'].
dataType(bool) --> ['bool'].

% Parsing if condition
if_cond(ifCondition(M, N)) --> ['if'], ['('], (condition(M);boolean(M)), [')'], block(N).
if_cond(ifCondition(M, N, O)) --> ['if'], ['('], (condition(M);boolean(M)), [')'], block(N), ['else'], block(O).

% Parsing for loop
for(forLoop(M, N, O, P)) --> 
    ['for'], ['['], declaration(M), [';'], (condition(N);boolean(N)), [';'], iterator(O), [']'], block(P).
for(forLoop(M, N, O, P)) --> 
    ['for'], ['['], declaration(M), [';'], (condition(N);boolean(N)), [';'], assignment(O), [']'], block(P).
for(forLoop(M, N, O, P)) --> 
    ['for'], ['['], assignment(M), [';'], (condition(N);boolean(N)), [';'], iterator(O), [']'], block(P).
for(forLoop(M, N, O, P)) --> 
    ['for'], ['['], assignment(M), [';'], (condition(N);boolean(N)), [';'], expression(O), [']'], block(P).

% Parsing forRange loop
for_in_range(forRange(M, N, O, P)) --> 
    ['for'], identifier(M), ['in'], ['range'], ['['], value(N), ['--'], value(O), [']'], block(P).
for_in_range(forRange(M, N, O, P)) --> 
    ['for'], identifier(M), ['in'], ['range'], ['['], identifier(N), ['--'], identifier(O), [']'], block(P).
for_in_range(forRange(M, N, O, P)) --> 
    ['for'], identifier(M), ['in'], ['range'], ['['], value(N), ['--'], identifier(O), [']'], block(P).
for_in_range(forRange(M, N, O, P)) --> 
    ['for'], identifier(M), ['in'], ['range'], ['['], identifier(N), ['--'], value(O), [']'], block(P).

% Parsing M while loop
while(whileLoop(M, N)) --> 
    ['while'], ['('], (condition(M); boolean(M)), [')'], block(N).

expression(add(M, N)) --> expression(M), ['+'], expression_helper(M).
expression(sub(M, N)) --> expression(M), ['-'], expression_helper(M).
expression(M) --> expression_helper(M).
expression_helper(mul(M, N)) --> expression_helper(M), ['*'], expression_helper(N).
expression_helper(div(M, N)) --> expression_helper(M), ['/'], expression_helper(N).
expression_helper(mod(M, E, N)) --> expression_helper(M),operator_helper(E),expression_helper(N).
expression_helper(square(M, E, N)) --> expression_helper(M),operator_helper(E),expression_helper(N).
expression_helper(M)--> ['('],expression(M),[')'].
expression_helper(M) --> value(M).
expression_helper(M) --> identifier(M).

operator_helper(\\) --> ['mod'].
operator_helper(^) --> ['**'].

iterator(increment(M)) --> identifier(M),['+'],['+'].
iterator(decrement(M)) --> identifier(M),['-'],['-'].

value(value(M)) --> [M],{number(M)}.
identifier(identifier(M)) --> [M], {atom(M)}.
string(M) --> onlystring(M).
onlystring(onlystring(M)) --> [M], {atom(M)}.


% Parsing boolean expressions
boolean(true) --> ['true'].
boolean(false) --> ['false'].
boolean(bool_Not(X)) --> ['not'],['('], boolean(X), [')'].
boolean(bool_Not(X)) --> ['not'],['('], condition(X), [')'].
boolean(bool_And(X, Y)) --> boolean(X), ['and'], boolean(Y).
boolean(bool_And(X, Y)) --> condition(X), ['and'], condition(Y).
boolean(bool_Or(X, Y)) --> boolean(X), ['or'], boolean(Y).
boolean(bool_Or(X, Y)) --> condition(X), ['or'], condition(Y).

not(true, false).
not(false, true).

and(false, _, false).
and(_, false, false).
and(true, true, true).

or(true, _, true).
or(_, true, true).
or(false, false, false).


%lookup predicate

lookup(Id, [(_Type, Id, Temp)|_], Temp).
lookup(Id, [_|Tail], Temp) :- lookup(Id, Tail, Temp).

lookup_type(Id, [_|Tail], Temp) :- lookup_type(Id, Tail, Temp).
lookup_type(Id, [(Type,Id,_X)|_], Type).

%update predicate updates the value of the identifier

update(Type, Id, Val, [], [(Type, Id, Val)]).
update(Type, Id, Val, [(Type, Id, _)|Tail], [(Type, Id, Val)|Tail]).
update(Type, Id, Val, [Head|Tail], [Head|Rest]) :- update(Type, Id, Val, Tail, Rest).

%Evaluations begin here

% Evaluate program statement
evalProgram(t_Program(X), FinalEnv) :- 
    evalBlock(X, [], FinalEnv), !.

% Evaluate block expressions
evalBlock(block(X), Env, FinalEnv) :- 
    evalBlockHelper(X, Env, FinalEnv).
evalBlockHelper(block(X, Y), Env, FinalEnv) :- 
    evalStatements(X, Env, Env1),  evalBlockHelper(Y, Env1, FinalEnv).
evalBlockHelper(block(X), Env, FinalEnv) :- evalStatements(X, Env, FinalEnv).

% Evaluate declaration statements
evalDeclare(declaration(X, Y), Env, NEnv):- 
    evalCharTree(Y, Id),
    update(X, Id, _, Env, NEnv).
evalDeclare(declaration(int, Y, Z), Env, NEnv):- 
    evalCharTree(Y, Id),
    evalExpression(Z, Env, Env1, Val),
    update(int, Id, Val, Env1, NEnv).
evalDeclare(declaration(string, Y, Z), Env, NEnv):- 
    evalCharTree(Y, Id),
    evalStr(Z, Env, NEnv1, Val),
    update(string, Id, Val, NEnv1, NEnv).
evalDeclare(declaration(bool, Y, true), Env, NEnv):- 
    evalCharTree(Y, Id),
    update(bool, Id, true, Env, NEnv).
evalDeclare(declaration(bool, Y, false), Env, NEnv):- 
    evalCharTree(Y, Id),
    update(bool, Id, false, Env, NEnv).

% Evaluate statement expressions
evalStatements(t_statements(X), Env, FinalEnv) :- 
    evalDeclare(X, Env, FinalEnv);
    evalAssignment(X, Env, FinalEnv);
    evalBoolean(X, Env, FinalEnv, _Val);
    evalPrint(X, Env, FinalEnv);
    evalCondition(X, Env, FinalEnv);
    evalWhile(X, Env, FinalEnv);
    evalFor(X, Env, FinalEnv);
    evalForInRange(X, Env, FinalEnv);
    evalTernary(X, Env, FinalEnv);
    evalIterator(X, Env, FinalEnv).

% Evaluate loop expressions
evalWhile(t_WhileLoop(X,Y), Env,FinalEnv):- 
    evalBoolean(X, Env, NEnv,true),
    evalBlock(Y, NEnv, NEnv1),
    evalWhile(t_WhileLoop(X,Y), NEnv1,FinalEnv).
evalWhile(t_WhileLoop(X,_Y), Env, Env) :- 
    evalBoolean(X, Env, Env,false).
evalWhile(t_WhileLoop(X,Y), Env,FinalEnv):- 
    evalCondition(X, Env, NEnv,true),
    evalBlock(Y, NEnv, NEnv1),
    evalWhile(t_WhileLoop(X,Y), NEnv1,FinalEnv).
evalWhile(t_WhileLoop(X,_Y), Env, Env) :- 
    evalCondition(X, Env, Env,false).

evalFor(t_ForLoop(X,Y,Z,W), Env,FinalEnv):- 
    evalDeclare(X, Env, NEnv),
    loops(Y,Z,W, NEnv,FinalEnv).
evalFor(t_ForLoop(X,Y,Z,W), Env,FinalEnv):- 
    eval_assign(X, Env, NEnv),
    loops(Y,Z,W, NEnv,FinalEnv).
loops(X,Y,Z, Env,FinalEnv) :- 
    evalCondition(X, Env, Env,true),
    evalBlock(Z, Env, NEnv),
    (evalIterator(Y, NEnv, NEnv1);evalExpression(Y, NEnv, NEnv1)),
    loops(X,Y,Z, NEnv1,FinalEnv).
loops(X,_Y,_Z, Env, Env) :- 
    evalCondition(X, Env, Env,false).
loops(X,Y,Z, Env,FinalEnv) :- 
    evalBoolean(X, Env, Env,true),
    evalBlock(Z, Env, NEnv),
    (evalIterator(Y, NEnv, NEnv1);evalExpression(Y, NEnv, NEnv1)),
    loops(X,Y,Z, NEnv1,FinalEnv).
loops(X,_Y,_Z, Env, Env) :- 
    evalBoolean(X, Env, Env,false).

evalForrange(forRange(X,Y,Z,W), Env,FinalEnv):- 
    evalCharTree(X,Id),
    ((evalNumtree(Y, Val),update(int,Id, Val, Env, NEnv));
    (lookup(Y, Env, Val),update(int,Id, Val, Env, NEnv))),
    ((evalNumtree(Z,N));
    (evalCharTree(Z,Id1),lookup(Id1, NEnv,N))),
    looping(Id,N,W, NEnv,FinalEnv).
looping(X,Z,W, Env,FinalEnv):- 
    lookup(X, Env, Val),
    Val < Z, 
    evalBlock(W, Env, NEnv),
    Val1 is Val + 1,
    update(int, X, Val1, NEnv, NEnv1),
    looping(X,Z,W, NEnv1,FinalEnv).
looping(X,Z,_W, Env, Env) :- 
    lookup(X, Env, Val), 
    Val >= Z.

% Evaluates boolean expressions
evalBoolean(true, _Env, _NEnv, true).
evalBoolean(false, _Env, _NEnv,false).
evalBoolean(t_bool_not(B), Env, NEnv, Val) :- 
    (evalBoolean(B, Env, NEnv, Val1);eval_condition(B, Env, NEnv, Val1)), 
    not(Val1, Val2), 
    Val = Val2.
evalBoolean(t_bool_and(X, Y), Env, NEnv, Val) :- 
    evalBoolean(X, Env, NEnv, Val1),
    evalBoolean(Y, Env, NEnv, Val2),
    and(Val1, Val2, Val).
evalBoolean(t_bool_and(X, Y), Env, NEnv, Val) :- 
    eval_condition(X, Env, NEnv, Val1),
    eval_condition(Y, Env, NEnv, Val2), 
    and(Val1, Val2, Val).
evalBoolean(t_bool_or(X, Y), Env, NEnv, Val) :- 
    evalBoolean(X, Env, NEnv, Val1),
    evalBoolean(Y, Env, NEnv, Val2),
    or(Val1, Val2, Val).
evalBoolean(t_bool_or(X, Y), Env, NEnv, Val) :- 
    eval_condition(X, Env, NEnv, Val1),
    eval_condition(Y, Env, NEnv, Val2),
    or(Val1, Val2, Val).

% Evaluate arithemetic expressions
evalExpression(M, Env, NEnv) :- 
    evalAssignment(M, Env, NEnv).
evalExpression(M, Env, NEnv, Val) :- 
    evalExpressionHelper(M, Env, NEnv, Val).
evalExpression(sub(M,N), Env, NEnv, Val):-
    evalExpression(M, Env, Env1, Val1),
    evalExpressionHelper(N, Env1, NEnv, Val2),
    Val is Val1 - Val2.
evalExpressionHelper(M, Env, NEnv, Val) :- 
    evalHelper1(M, Env, NEnv, Val).
evalExpressionHelper(add(M,N), Env, NEnv, Val):-
    evalExpressionHelper(M, Env, Env1, Val1),
    evalHelper1(N, Env1, NEnv, Val2),
    Val is Val1 + Val2.
evalHelper1(M, Env, NEnv, Val) :- 
    evalHelper2(M, Env, NEnv, Val).
evalHelper1(mult(M,N), Env, NEnv, Val):-
    evalHelper1(M, Env, Env1, Val1),
    evalHelper2(N, Env1, NEnv, Val2),
    Val is Val1 * Val2.
evalHelper2(M, Env, NEnv, Val) :- 
    evalHelper3(M, Env, NEnv, Val).
evalHelper2(div(M,N),  Env, NEnv, Val):-
    evalHelper2(M, Env, Env1, Val1), 
    evalHelper3(N, Env1, NEnv, Val2),
    Val is floor(Val1 / Val2).
evalHelper3(M,  Env, NEnv, Val) :- 
    evalValue(M, Env, NEnv, Val).
evalHelper3(t_parentheses(M), Env, NEnv, Val):-
    evalExpression(M, Env, NEnv, Val).

%to evaluate the increment,decrement operation
evalIterator(increment(M), Env, NEnv) :- 
    evalCharTree(M,Id),
    lookup_type(Id, Env,int),
    lookup(Id, Env, Val),
    Val1 is Val + 1, 
    update(int,Id, Val1, Env, NEnv).
evalIterator(decrement(M), Env, NEnv) :- 
    evalCharTree(M,Id),
    lookup_type(Id, Env,int),
    lookup(Id, Env, Val),
    Val1 is Val - 1, 
    update(int,Id, Val1, Env, NEnv).

evalValue(value(Val), Env, Env, Val).
evalValue(identifier(I), Env, Env, Val) :-
    term_to_atom(Id,I),
    lookup(Id, Env, Val).
evalValueTree(value(Val), Val).
evalCharTree(identifier(I),Id):- 
    term_to_atom(Id,I).
evalStr(string(I), Env, Env, Val) :- 
    atom_string(I, Val).

% Evaluate conditional expressions
evalIf(ifCondition(M,N), Env,FinalEnv):- 
    ((eval_condition(M, Env, NEnv,true);evalBoolean(M, Env, NEnv,true)),evalBlock(N, NEnv,FinalEnv)).
evalIf(ifCondition(M,_N), Env, NEnv):- 
    eval_condition(M, Env, NEnv,false);evalBoolean(M, Env, NEnv,false).
evalIf(ifCondition(M,N,_O), Env,FinalEnv):- 
    (eval_condition(M, Env, NEnv,true);evalBoolean(M, Env, NEnv,true)),
    evalBlock(N, NEnv,FinalEnv).
evalIf(ifCondition(M,_N,O), Env,FinalEnv):- 
    (eval_condition(M, Env, NEnv,false);evalBoolean(M, Env, NEnv,false)),
    evalBlock(O, NEnv,FinalEnv).