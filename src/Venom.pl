venom(Lexername, Filename) :-
    term_to_atom(N, M),
    program(Tree, N, []),
    evalProgram(Tree, Output).

:- table boolean/3, expression/3, term/3.

% Parsing the program 
program(program(M)) -->['begin'], block(M), ['end'].

% Parsing the block
block(block(M)) --> ['{'], blockSection(M), ['}']. 
blockSection(block(M)) --> statements(M).
blockSection(block(M, N)) --> statements(M), blockSection(N).

% Parsing statements 
statements(statements(M)) --> 
    declaration(M), [;].
statements(statements(M)) --> 
    assignment(M), [;].
statements(statements(M)) --> 
    expression(M), [;].
statements(statements(M)) --> 
    boolean(M), [;].
statements(statements(M)) --> 
    printstatements(M), [;].
statements(statements(M)) --> 
    if_cond(M).
statements(statements(M)) --> 
    ternarycondition(M), [;].
statements(statements(M)) --> 
    for(M).
statements(statements(M)) --> 
    while(M).
statements(statements(M)) --> 
    for_in_range(M).
statements(statements(M)) --> 
    iterator(M), [;].

%to parse variable declaration
declaration(declare(int, M, N)) --> ['int'], identifier(M), ['='], expression(N).
declaration(declare(string, M, N)) --> ['string'], identifier(M), ['='], string(N).
declaration(declare(boolean, M, true)) --> ['bool'], identifier(M), [=], ['true'].
declaration(declare(boolean, M, false)) --> ['bool'], identifier(M), [=], ['false'].
declaration(declare(M, N)) --> dataType(M), identifier(N).

% Parsing datatypes
dataType(int) --> ['int'].
dataType(string) --> ['string'].
dataType(bool) --> ['bool'].

% Parsing assignments
assignment(assignment(M, N)) --> identifier(M), ['='], expression(N) | identifier(M), ['='], boolean(N).

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
boolean(bool_Not(M)) --> ['not'],['('], boolean(M), [')'].
boolean(bool_Not(M)) --> ['not'],['('], condition(M), [')'].
boolean(bool_And(M, N)) --> boolean(M), ['and'], boolean(N).
boolean(bool_And(M, N)) --> condition(M), ['and'], condition(N).
boolean(bool_Or(M, N)) --> boolean(M), ['or'], boolean(N).
boolean(bool_Or(M, N)) --> condition(M), ['or'], condition(N).

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

lookupHelper(Id, [_|Tail], Temp) :- lookupHelper(Id, Tail, Temp).
lookupHelper(Id, [(Type,Id,_M)|_], Type).

%update predicate updates the value of the identifier

update(Type, Id, Val, [], [(Type, Id, Val)]).
update(Type, Id, Val, [(Type, Id, _)|Tail], [(Type, Id, Val)|Tail]).
update(Type, Id, Val, [Head|Tail], [Head|Rest]) :- update(Type, Id, Val, Tail, Rest).

%Evaluations begin here

% Evaluate program statement
evalProgram(t_Program(M), FinalEnv) :- 
    evalBlock(M, [], FinalEnv), !.

% Evaluate block expressions
evalBlock(block(M), Env, FinalEnv) :- 
    evalBlockHelper(M, Env, FinalEnv).
evalBlockHelper(block(M, N), Env, FinalEnv) :- 
    evalStatements(M, Env, Env1),  evalBlockHelper(N, Env1, FinalEnv).
evalBlockHelper(block(M), Env, FinalEnv) :- evalStatements(M, Env, FinalEnv).

% Evaluate declaration statements
evalDeclare(declaration(M, N), Env, NEnv):- 
    evalCharTree(N, Id),
    update(M, Id, _, Env, NEnv).
evalDeclare(declaration(int, N, O), Env, NEnv):- 
    evalCharTree(N, Id),
    evalExpression(O, Env, Env1, Val),
    update(int, Id, Val, Env1, NEnv).
evalDeclare(declaration(string, N, O), Env, NEnv):- 
    evalCharTree(N, Id),
    evalStr(O, Env, NEnv1, Val),
    update(string, Id, Val, NEnv1, NEnv).
evalDeclare(declaration(bool, N, true), Env, NEnv):- 
    evalCharTree(N, Id),
    update(bool, Id, true, Env, NEnv).
evalDeclare(declaration(bool, N, false), Env, NEnv):- 
    evalCharTree(N, Id),
    update(bool, Id, false, Env, NEnv).

% Evaluate statement expressions
evalStatements(t_statements(M), Env, FinalEnv) :- 
    evalDeclare(M, Env, FinalEnv);
    evalAssignment(M, Env, FinalEnv);
    evalBoolean(M, Env, FinalEnv, _Val);
    evalPrint(M, Env, FinalEnv);
    evalCondition(M, Env, FinalEnv);
    evalWhile(M, Env, FinalEnv);
    evalFor(M, Env, FinalEnv);
    evalForInRange(M, Env, FinalEnv);
    evalTernary(M, Env, FinalEnv);
    evalIterator(M, Env, FinalEnv).

% Evaluating assignments
evalAssignment(assignment(M, N), Env, NEnv) :- 
    evalExpression(N, Env, Env1, Val),
    check_type(Val, T),
    evalCharTree(M, Id),
    lookupHelper(Id, Env1, T1),
    T =@= T1,
    update(T, Id, Val, Env1, NEnv).
evalAssignment(assignment(M, N), Env, NEnv) :- 
    evalStr(N, Env, Env, Val),
    check_type(Val, T),
    evalCharTree(M, Id),
    lookupHelper(Id, Env, T1),
    T =@= T1,
    update(T, Id, Val, Env, NEnv).
evalAssignment(assignment(M, N), Env, NEnv) :- 
   evalBoolean(N, Env, Env, Val),
    check_type(Val, T),
    evalCharTree(M, Id),
   lookupHelper(Id, Env, T1),
    T =@= T1,
    update(T, Id, Val, Env, NEnv).

% Evaluate loop expressions
evalWhile(t_WhileLoop(M,N), Env,FinalEnv):- 
    evalBoolean(M, Env, NEnv,true),
    evalBlock(N, NEnv, NEnv1),
    evalWhile(t_WhileLoop(M,N), NEnv1,FinalEnv).
evalWhile(t_WhileLoop(M,_N), Env, Env) :- 
    evalBoolean(M, Env, Env,false).
evalWhile(t_WhileLoop(M,N), Env,FinalEnv):- 
    evalCondition(M, Env, NEnv,true),
    evalBlock(N, NEnv, NEnv1),
    evalWhile(t_WhileLoop(M,N), NEnv1,FinalEnv).
evalWhile(t_WhileLoop(M,_N), Env, Env) :- 
    evalCondition(M, Env, Env,false).

evalFor(t_ForLoop(M,N,O,W), Env,FinalEnv):- 
    evalDeclare(M, Env, NEnv),
    loops(N,O,W, NEnv,FinalEnv).
evalFor(t_ForLoop(M,N,O,W), Env,FinalEnv):- 
    evalAssignment(M, Env, NEnv),
    loops(N,O,W, NEnv,FinalEnv).
loops(M,N,O, Env,FinalEnv) :- 
    evalCondition(M, Env, Env,true),
    evalBlock(O, Env, NEnv),
    (evalIterator(N, NEnv, NEnv1);evalExpression(N, NEnv, NEnv1)),
    loops(M,N,O, NEnv1,FinalEnv).
loops(M,_N,_O, Env, Env) :- 
    evalCondition(M, Env, Env,false).
loops(M,N,O, Env,FinalEnv) :- 
    evalBoolean(M, Env, Env,true),
    evalBlock(O, Env, NEnv),
    (evalIterator(N, NEnv, NEnv1);evalExpression(N, NEnv, NEnv1)),
    loops(M,N,O, NEnv1,FinalEnv).
loops(M,_N,_O, Env, Env) :- 
    evalBoolean(M, Env, Env,false).

evalForrange(forRange(M,N,O,W), Env,FinalEnv):- 
    evalCharTree(M,Id),
    ((evalNumtree(N, Val),update(int,Id, Val, Env, NEnv));
    (lookup(N, Env, Val),update(int,Id, Val, Env, NEnv))),
    ((evalNumtree(O,N));
    (evalCharTree(O,Id1),lookup(Id1, NEnv,N))),
    looping(Id,N,W, NEnv,FinalEnv).
looping(M,O,W, Env,FinalEnv):- 
    lookup(M, Env, Val),
    Val < O, 
    evalBlock(W, Env, NEnv),
    Val1 is Val + 1,
    update(int, M, Val1, NEnv, NEnv1),
    looping(M,O,W, NEnv1,FinalEnv).
looping(M,O,_W, Env, Env) :- 
    lookup(M, Env, Val), 
    Val >= O.

% Evaluates boolean expressions
evalBoolean(true, _Env, _NEnv, true).
evalBoolean(false, _Env, _NEnv,false).
evalBoolean(t_bool_not(B), Env, NEnv, Val) :- 
    (evalBoolean(B, Env, NEnv, Val1);eval_condition(B, Env, NEnv, Val1)), 
    not(Val1, Val2), 
    Val = Val2.
evalBoolean(t_bool_and(M, N), Env, NEnv, Val) :- 
    evalBoolean(M, Env, NEnv, Val1),
    evalBoolean(N, Env, NEnv, Val2),
    and(Val1, Val2, Val).
evalBoolean(t_bool_and(M, N), Env, NEnv, Val) :- 
    eval_condition(M, Env, NEnv, Val1),
    eval_condition(N, Env, NEnv, Val2), 
    and(Val1, Val2, Val).
evalBoolean(t_bool_or(M, N), Env, NEnv, Val) :- 
    evalBoolean(M, Env, NEnv, Val1),
    evalBoolean(N, Env, NEnv, Val2),
    or(Val1, Val2, Val).
evalBoolean(t_bool_or(M, N), Env, NEnv, Val) :- 
    eval_condition(M, Env, NEnv, Val1),
    eval_condition(N, Env, NEnv, Val2),
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
    lookupHelper(Id, Env,int),
    lookup(Id, Env, Val),
    Val1 is Val + 1, 
    update(int,Id, Val1, Env, NEnv).
evalIterator(decrement(M), Env, NEnv) :- 
    evalCharTree(M,Id),
    lookupHelper(Id, Env,int),
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