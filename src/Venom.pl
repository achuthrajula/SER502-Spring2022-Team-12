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

%to evaluate the program
evalProgram(t_Program(X), FinalEnv) :- 
    evalBlock(X, [], FinalEnv), !.

%to evaluate the block
evalBlock(block(X), Env, FinalEnv) :- 
    evalBlockHelper(X, Env, FinalEnv).
evalBlockHelper(block(X, Y), Env, FinalEnv) :- 
    evalStatements(X, Env, Env1),  evalBlockHelper(Y, Env1, FinalEnv).
evalBlockHelper(block(X), Env, FinalEnv) :- evalStatements(X, Env, FinalEnv).

%to evaluate different types of declaration
evalDeclare(declaration(X, Y), Env, NewEnv):- 
    evalCharTree(Y, Id),
    update(X, Id, _, Env, NewEnv).
evalDeclare(declaration(int, Y, Z), Env, NewEnv):- 
    evalCharTree(Y, Id),
    evalExpression(Z, Env, Env1, Val),
    update(int, Id, Val, Env1, NewEnv).
evalDeclare(declaration(string, Y, Z), Env, NewEnv):- 
    evalCharTree(Y, Id),
    evalStr(Z, Env, NewEnv1, Val),
    update(string, Id, Val, NewEnv1, NewEnv).
evalDeclare(declaration(bool, Y, true), Env, NewEnv):- 
    evalCharTree(Y, Id),
    update(bool, Id, true, Env, NewEnv).
evalDeclare(declaration(bool, Y, false), Env, NewEnv):- 
    evalCharTree(Y, Id),
    update(bool, Id, false, Env, NewEnv).

%to evaluate the while loop
evalWhile(t_WhileLoop(X,Y), Env,FinalEnv):- 
    evalBoolean(X, Env, NewEnv,true),
    evalBlock(Y, NewEnv, NewEnv1),
    evalWhile(t_WhileLoop(X,Y), NewEnv1,FinalEnv).
evalWhile(t_WhileLoop(X,_Y), Env, Env) :- 
    evalBoolean(X, Env, Env,false).
evalWhile(t_WhileLoop(X,Y), Env,FinalEnv):- 
    evalCondition(X, Env, NewEnv,true),
    evalBlock(Y, NewEnv, NewEnv1),
    evalWhile(t_WhileLoop(X,Y), NewEnv1,FinalEnv).
evalWhile(t_WhileLoop(X,_Y), Env, Env) :- 
    evalCondition(X, Env, Env,false).

%to evaluate the forLoop
evalForloop(t_ForLoop(X,Y,Z,W), Env,FinalEnv):- 
    evalDeclare(X, Env, NewEnv),
    loops(Y,Z,W, NewEnv,FinalEnv).
evalForloop(t_ForLoop(X,Y,Z,W), Env,FinalEnv):- 
    eval_assign(X, Env, NewEnv),
    loops(Y,Z,W, NewEnv,FinalEnv).
loops(X,Y,Z, Env,FinalEnv) :- 
    evalCondition(X, Env, Env,true),
    evalBlock(Z, Env, NewEnv),
    (evalIterator(Y, NewEnv, NewEnv1);evalExpression(Y, NewEnv, NewEnv1)),
    loops(X,Y,Z, NewEnv1,FinalEnv).
loops(X,_Y,_Z, Env, Env) :- 
    evalCondition(X, Env, Env,false).
loops(X,Y,Z, Env,FinalEnv) :- 
    evalBoolean(X, Env, Env,true),
    evalBlock(Z, Env, NewEnv),
    (evalIterator(Y, NewEnv, NewEnv1);evalExpression(Y, NewEnv, NewEnv1)),
    loops(X,Y,Z, NewEnv1,FinalEnv).
loops(X,_Y,_Z, Env, Env) :- 
    evalBoolean(X, Env, Env,false).

%to evaluate the forRange
evalForrange(forRange(X,Y,Z,W), Env,FinalEnv):- 
    evalCharTree(X,Id),
    ((evalNumtree(Y, Val),update(int,Id, Val, Env, NewEnv));
    (lookup(Y, Env, Val),update(int,Id, Val, Env, NewEnv))),
    ((evalNumtree(Z,N));
    (evalCharTree(Z,Id1),lookup(Id1, NewEnv,N))),
    looping(Id,N,W, NewEnv,FinalEnv).
looping(X,Z,W, Env,FinalEnv):- 
    lookup(X, Env, Val),
    Val < Z, 
    evalBlock(W, Env, NewEnv),
    Val1 is Val + 1,
    update(int, X, Val1, NewEnv, NewEnv1),
    looping(X,Z,W, NewEnv1,FinalEnv).
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

%to evaluate addition,subtraction,multiplication and division
evalExpression(X, Env, NewEnv) :- 
    eval_assignment(X, Env, NewEnv).
evalExpression(X, Env, NewEnv, Val) :- 
    evalExpressionHelper(X, Env, NewEnv, Val).
evalExpression(sub(X,Y), Env, NewEnv, Val):-
    evalExpression(X, Env, Env1, Val1),
    evalExpressionHelper(Y, Env1, NewEnv, Val2),
    Val is Val1 - Val2.
evalExpressionHelper(X, Env, NewEnv, Val) :- 
    evalHelper1(X, Env, NewEnv, Val).
evalExpressionHelper(add(X,Y), Env, NewEnv, Val):-
    evalExpressionHelper(X, Env, Env1, Val1),
    evalHelper1(Y, Env1, NewEnv, Val2),
    Val is Val1 + Val2.
evalHelper1(X, Env, NewEnv, Val) :- 
    evalHelper2(X, Env, NewEnv, Val).
evalHelper1(mult(X,Y), Env, NewEnv, Val):-
    evalHelper1(X, Env, Env1, Val1),
    evalHelper2(Y, Env1, NewEnv, Val2),
    Val is Val1 * Val2.
evalHelper2(X, Env, NewEnv, Val) :- 
    evalHelper3(X, Env, NewEnv, Val).
evalHelper2(div(X,Y),  Env, NewEnv, Val):-
    evalHelper2(X, Env, Env1, Val1), 
    evalHelper3(Y, Env1, NewEnv, Val2),
    Val is floor(Val1 / Val2).
evalHelper3(X,  Env, NewEnv, Val) :- 
    evalValue(X, Env, NewEnv, Val).
evalHelper3(t_parentheses(X), Env, NewEnv, Val):-
    evalExpression(X, Env, NewEnv, Val).

%to evaluate the increment,decrement operation
evalIterator(increment(M), Env, NewEnv) :- 
    evalCharTree(M,Id),
    lookup_type(Id, Env,int),
    lookup(Id, Env, Val),
    Val1 is Val + 1, 
    update(int,Id, Val1, Env, NewEnv).
evalIterator(decrement(M), Env, NewEnv) :- 
    evalCharTree(M,Id),
    lookup_type(Id, Env,int),
    lookup(Id, Env, Val),
    Val1 is Val - 1, 
    update(int,Id, Val1, Env, NewEnv).

evalValue(value(Val), Env, Env, Val).
evalValue(identifier(I), Env, Env, Val) :-
    term_to_atom(Id,I),
    lookup(Id, Env, Val).
evalValueTree(value(Val), Val).
evalCharTree(identifier(I),Id):- 
    term_to_atom(Id,I).
evalStr(string(I), Env, Env, Val) :- 
    atom_string(I, Val).

% Evaluate if condition

evalIf(ifCondition(M,N), Env,FinalEnv):- 
    ((eval_condition(M, Env, NewEnv,true);evalBoolean(M, Env, NewEnv,true)),evalBlock(N, NewEnv,FinalEnv)).
evalIf(ifCondition(M,_N), Env, NewEnv):- 
    eval_condition(M, Env, NewEnv,false);evalBoolean(M, Env, NewEnv,false).
evalIf(ifCondition(M,N,_O), Env,FinalEnv):- 
    (eval_condition(M, Env, NewEnv,true);evalBoolean(M, Env, NewEnv,true)),
    evalBlock(N, NewEnv,FinalEnv).
evalIf(ifCondition(M,_N,O), Env,FinalEnv):- 
    (eval_condition(M, Env, NewEnv,false);evalBoolean(M, Env, NewEnv,false)),
    evalBlock(O, NewEnv,FinalEnv).