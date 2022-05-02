:- table boolean/3, expression/3, term/3.
:- set_prolog_flag(singleton,off).

venom(Lexer, Filename) :-
    string_concat(Lexer, '/venom/src/lexer.py', X), writeln(X),
    process_create(path('python3.8'), [X, Filename], [stdout(pipe(In))]),
    read_string(In, _, M),
    term_to_atom(N, M),
    program(Tree, N, []),
    write('List of Tokens:'), nl, write(N),nl, nl,
    write('Parse Tree:'), nl, write(Tree),nl, nl, write('Output:'), nl,
    evalProgram(Tree, _Output).

:- table boolean/3, expression/3, term/3.
:- set_prolog_flag(singleton,off).

%to parse the program 
program(program(M)) -->['start'], block(M), ['end'].

%to parse the block 
block(block(M)) --> ['{'], blockHelper(M), ['}']. 
blockHelper(block(M, N)) --> statements(M), blockHelper(N).
blockHelper(block(M)) --> statements(M).

%to parse the different type of statements 
statements(statements(M)) --> declaration(M), [;].
statements(statements(M)) --> assignment(M), [;].
statements(statements(M)) --> expression(M), [;].
statements(statements(M)) --> boolean(M), [;].
statements(statements(M)) --> outputStream(M), [;].
statements(statements(M)) --> if_cond(M).
statements(statements(M)) --> ternaryExpression(M), [;].
statements(statements(M)) --> for(M).
statements(statements(M)) --> while(M).
statements(statements(M)) --> forInRange(M).
statements(statements(M)) --> iterator(M), [;].

%to parse variable declaration
declaration(declaration(int, M, N)) --> ['int'], identifier(M), ['='], expression(N).
declaration(declaration(string, M, N)) --> ['string'], identifier(M), ['='], string(N).
declaration(declaration(bool, M, true)) --> ['bool'], identifier(M), [=], ['true'].
declaration(declaration(bool, M, false)) --> ['bool'], identifier(M), [=], ['false'].
declaration(declaration(M, N)) --> type(M), identifier(N).

%to parse assignment operation
assignment(assignment(M, N)) --> 
        identifier(M), ['='], expression(N) 
        | identifier(M), ['='], boolean(N).

%to parse datatype
type(int) --> ['int'].
type(string) --> ['string'].
type(bool) --> ['bool'].

%to parse while
while(while(M, N)) --> ['while'], ['('], (condition(M);boolean(M)), [')'], block(N).

%to parse for
for(forLoop(M, N, O, D)) --> 
        ['for'], ['('], declaration(M), [';'], (condition(N);boolean(N)), [';'], iterator(O), [')'], block(D)
        | ['for'], ['('], declaration(M), [';'], (condition(N);boolean(N)), [';'], assignment(O), [')'], block(D)
        | ['for'], ['('], assignment(M), [';'], (condition(N);boolean(N)), [';'], iterator(O), [')'], block(D)
        | ['for'], ['('], assignment(M), [';'], (condition(N);boolean(N)), [';'], expression(O), [')'], block(D).

%to parse forInRange
forInRange(forRange(M, N, O, D)) --> 
        ['for'], identifier(M), ['in'], ['range'], ['('], value(N), [':'], value(O), [')'], block(D)
        | ['for'], identifier(M), ['in'], ['range'], ['('], identifier(N), [':'], identifier(O), [')'], block(D)
        | ['for'], identifier(M), ['in'], ['range'], ['('], value(N), [':'], identifier(O), [')'], block(D)
        | ['for'], identifier(M), ['in'], ['range'], ['('], identifier(N), [':'], value(O), [')'], block(D).

%to parse if condition
if_cond(ifCondition(M, N)) --> ['if'], ['('], (condition(M);boolean(M)), [')'], block(N).
if_cond(ifCondition(M, N, O)) --> ['if'], ['('], (condition(M);boolean(M)), [')'], block(N), ['else'], block(O).

%to parse ternary condition
ternaryExpression(ternary(M, N, O)) --> (condition(M);boolean(M)), ['?'], statements(N), [':'], statements(O).

%to parse the boolean expression 
boolean(true) --> ['true'].
boolean(false) --> ['false'].
boolean(boolNot(M)) --> 
        ['not'],['('], boolean(M), [')']
        | ['not'],['('], condition(M), [')'].
boolean(boolAnd(M, N)) -->
        boolean(M), ['and'], boolean(N)
        | condition(M), ['and'], condition(N).
boolean(boolOr(M, N)) -->
        boolean(M), ['or'], boolean(N)
        | condition(M), ['or'], condition(N).

%to parse flash statements
outputStream(flash(M)) --> 
        ['flash'], identifier(M)
        | ['flash'], value(M)
        | ['flash'], string(M).

%to parse condition checks
condition(condition(M, N, O)) --> expression(M), operator(N), expression(O)
    | string(M), operator(N), string(O)
    | identifier(M), operator(N), string(O).

%to parse conditional operator
operator(==) --> ['=='].
operator('!=') --> ['!='].
operator(>) --> ['>'].
operator(<) --> ['<'].
operator(>=) --> ['>='].
operator(<=) --> ['<='].

%to parse addition ,subtraction,multiplication and division
expression(add(M, N)) --> expression(M), ['+'], term(N).
expression(sub(M, N)) --> expression(M), ['-'], term(N).
expression(M) --> term(M).
term(mul(M, N)) --> term(M), ['*'], term(N).
term(div(M, N)) --> term(M), ['/'], term(N).
term(M) --> ['('], expression(M), [')'].
term(M) --> value(M).
term(M) --> identifier(M).

%to parse unary increment and decrement operation
iterator(increment(M)) --> identifier(M), ['+'], ['+'] .
iterator(decrement(M)) --> identifier(M), ['-'], ['-'].

%to parse number, identifier, and string
value(value(N)) --> [N], {number(N)}.
identifier(identifier(N)) --> [N], {atom(N)}.
string(N) --> onlystring(N).
onlystring(string(N)) --> [N], {atom(N)}.

typeCheck(Val, Temp) :- string(Val), Temp = string 
    | integer(Val), Temp = int
    | (Val = true ; Val = false), Temp = bool.

not(true, false).
not(false, true).

and(false, _, false).
and(_, false, false).
and(true, true, true).

or(true, _, true).
or(_, true, true).
or(false, false, false).

%lookup predicate find the respective values from the environment

lookup(Id, [(_Type, Id, Temp)|_], Temp).
lookup(Id, [_|Tail], Temp) :- lookup(Id, Tail, Temp).

lookupHelper(Id, [_|Tail], Temp) :- lookupHelper(Id, Tail, Temp).
lookupHelper(Id, [(Type,Id,_M)|_], Type).

%update predicate updates the value of the identifier

update(Type, Id, Val, [], [(Type, Id, Val)]).
update(Type, Id, Val, [(Type, Id, _)|Tail], [(Type, Id, Val)|Tail]).
update(Type, Id, Val, [Head|Tail], [Head|Rest]) :- update(Type, Id, Val, Tail, Rest).

%to evaluate the program
evalProgram(program(M), End) :- evalBlock(M, [], End), !.

%to evaluate the block
evalBlock(block(M), Env, End) :- evalBlockHelper(M, Env, End).
evalBlockHelper(block(M, N), Env, End) :- evalStatements(M, Env, Env1), 
    evalBlockHelper(N, Env1, End).
evalBlockHelper(block(M), Env, End) :- evalStatements(M, Env, End).

%to evaluate the statements
evalStatements(statements(M), Env, End) :- 
    evalDeclare(M, Env, End);
    evalAssignment(M, Env, End);
    evalBoolean(M, Env, End, _Val);
    evalFlash(M, Env, End);
    evalIf(M, Env, End);
    evalWhile(M, Env, End);
    evalFor(M, Env, End);
    evalForInRange(M, Env, End);
    evalTernary(M, Env, End);
    evalIterator(M, Env, End).

%to evaluate different types of declaration
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

%to evaluate the assignment operation
evalAssignment(assignment(M, N), Env, NEnv) :- 
    evalExpression(N, Env, Env1, Val),
    typeCheck(Val, T),
    evalCharTree(M, Id),
    lookupHelper(Id, Env1, T1),
    T =@= T1,
    update(T, Id, Val, Env1, NEnv).
evalAssignment(assignment(M, N), Env, NEnv) :- 
    evalStr(N, Env, Env, Val),
    typeCheck(Val, T),
    evalCharTree(M, Id),
    lookupHelper(Id, Env, T1),
    T =@= T1,
    update(T, Id, Val, Env, NEnv).
evalAssignment(assignment(M, N), Env, NEnv) :- 
   evalBoolean(N, Env, Env, Val),
    typeCheck(Val, T),
    evalCharTree(M, Id),
   lookupHelper(Id, Env, T1),
    T =@= T1,
    update(T, Id, Val, Env, NEnv).

%to evaluate boolean condition
evalBoolean(true, _Env1, _NEnv, true).
evalBoolean(false, _Env1, _NEnv,false).
evalBoolean(boolNot(B), Env, NEnv, Val) :- 
    (evalBoolean(B, Env, NEnv, Val1);evalCondition(B, Env, NEnv, Val1)), 
    not(Val1, Val2), 
    Val = Val2.
evalBoolean(boolAnd(M, N), Env, NEnv, Val) :- 
    evalBoolean(M, Env, NEnv, Val1),
    evalBoolean(N, Env, NEnv, Val2),
    and(Val1, Val2, Val).
evalBoolean(boolAnd(M, N), Env, NEnv, Val) :- 
    evalCondition(M, Env, NEnv, Val1),
    evalCondition(N, Env, NEnv, Val2), 
    and(Val1, Val2, Val).
evalBoolean(boolOr(M, N), Env, NEnv, Val) :- 
    evalBoolean(M, Env, NEnv, Val1),
    evalBoolean(N, Env, NEnv, Val2),
    or(Val1, Val2, Val).
evalBoolean(boolOr(M, N), Env, NEnv, Val) :- 
    evalCondition(M, Env, NEnv, Val1),
    evalCondition(N, Env, NEnv, Val2),
    or(Val1, Val2, Val).

%to evaluate conditional operation
evalCondition(condition(M, ==, N), Env, NEnv, Val) :- 
    evalExpression(M, Env, NEnv, Val1),
    evalExpression(N, Env, NEnv, Val2),
    (( Val1 =:= Val2, Val = true); ( \+(Val1 =:= Val2), Val = false)).
evalCondition(condition(M, '!=', N), Env, NEnv, Val) :- 
    evalExpression(M, Env, NEnv, Val1),
    evalExpression(N, Env, NEnv, Val2),
    (( Val1 =\= Val2, Val = true);( \+(Val1 =\= Val2), Val = false)).
evalCondition(condition(M, '>', N), Env, NEnv, Val) :-
    evalExpression(M, Env, NEnv, Val1),
    evalExpression(N, Env, NEnv, Val2),
    (( Val1 > Val2, Val = true);( \+(Val1 > Val2), Val = false)).
evalCondition(condition(M, '<', N), Env, NEnv, Val) :- 
    evalExpression(M, Env, NEnv, Val1),
    evalExpression(N, Env, NEnv, Val2),
    (( Val1 < Val2, Val = true);( \+(Val1 < Val2), Val = false)).
evalCondition(condition(M, '>=', N), Env, NEnv, Val) :- 
    evalExpression(M, Env, NEnv, Val1),
    evalExpression(N, Env, NEnv, Val2),
    (( Val1 >= Val2, Val = true);( \+(Val1 >= Val2), Val = false)).
evalCondition(condition(M, '<=', N), Env, NEnv, Val) :- 
    evalExpression(M, Env, NEnv, Val1),
    evalExpression(N, Env, NEnv, Val2),
    (( Val1 =< Val2, Val = true);( \+(Val1 =< Val2), Val = false)).
evalCondition(condition(M, ==, N), Env, NEnv, Val) :- 
    evalStr(M, Env, NEnv, Val1),
    evalStr(N, Env, NEnv, Val2),
    ((Val1 = Val2, Val = true);(\+(Val1 = Val2), Val = false)).
evalCondition(condition(M,'!=',N), Env, NEnv, Val) :-
    evalStr(M, Env, NEnv, Val1),
    evalStr(N, Env, NEnv, Val2),
    ((Val1 = Val2, Val = false);(\+(Val1 = Val2), Val = true)).
evalCondition(condition(M,'>',N), Env, NEnv,_Val) :- 
    evalStr(M, Env, NEnv,_Val1),
    evalStr(N, Env, NEnv,_Val2),
    write("invalid operation").
evalCondition(condition(M,'<',N), Env, NEnv,_Val) :- 
    evalStr(M, Env, NEnv,_Val1),
    evalStr(N, Env, NEnv,_Val2),
    write("invalid operation").
evalCondition(condition(M,'>=',N), Env, NEnv,_Val) :- 
    evalStr(M, Env, NEnv,_Val1),
    evalStr(N, Env, NEnv,_Val2),
    write("invalid operation").
evalCondition(condition(M,'<=',N), Env, NEnv,_Val) :- 
    evalStr(M, Env, NEnv,_Val1),
    evalStr(N, Env, NEnv,_Val2),
    write("invalid operation").
evalCondition(condition(M,==,N), Env, NEnv, Val) :-
    evalCharTree(M,Id),
    lookup(Id, Env, Val1),
    typeCheck(Val1,T),
    T=string,
    evalStr(N, Env, NEnv, Val2),
    ((Val1 =@= Val2, Val = true);(\+(Val1 =@= Val2), Val = false)).
evalCondition(condition(M,'!=',N), Env, NEnv, Val) :- 
    evalCharTree(M,Id),
    lookup(Id, Env, Val1),
    typeCheck(Val1,T),
    T=string,
    evalStr(N, Env, NEnv, Val2),
    ((Val1 = Val2, Val = false);(\+(Val1 = Val2), Val = true)).
evalCondition(condition(M,'>',N), Env, NEnv,_Val) :- 
    evalCharTree(M,Id),
    lookup(Id, Env, Val1),
    typeCheck(Val1,T),
    T=string,
    evalStr(N, Env, NEnv,_Val2),
    write("invalid operation").
evalCondition(condition(M,'<',N), Env, NEnv,_Val) :- 
    evalCharTree(M,Id),
    lookup(Id, Env, Val1),
    typeCheck(Val1,T),
    T=string,
    evalStr(N, Env, NEnv,_Val2),
    write("invalid operation").
evalCondition(condition(M,'>=',N), Env, NEnv,_Val) :- 
    evalCharTree(M,Id),
    lookup(Id, Env, Val1),
    typeCheck(Val1,T),
    T=string,
    evalStr(N, Env, NEnv,_Val2),
    write("invalid operation").
evalCondition(condition(M,'<=',N), Env, NEnv,_Val) :- 
    evalCharTree(M,Id),
    lookup(Id, Env, Val1),
    typeCheck(Val1,T),
    T=string,
    evalStr(N, Env, NEnv,_Val2),
    write("invalid operation").

%to evaluate the flash statement
evalFlash(flash(M), Env, Env) :- 
    evalCharTree(M,Id),
    lookup(Id, Env, Val),
    writeln(Val).
evalFlash(flash(M), Env, Env) :- 
    evalValueTree(M, Val),
    writeln(Val).
evalFlash(flash(M), Env, Env) :- 
    evalStr(M, Env, Env, Val),
    writeln(Val).

%to evaluate if condition
evalIf(ifCondition(M,N), Env,End):- 
    ((evalCondition(M, Env, NEnv,true);evalBoolean(M, Env, NEnv,true)),evalBlock(N, NEnv,End)).
evalIf(ifCondition(M,_N), Env, NEnv):- 
    evalCondition(M, Env, NEnv,false);evalBoolean(M, Env, NEnv,false).
evalIf(ifCondition(M,N,_O), Env,End):- 
    (evalCondition(M, Env, NEnv,true);evalBoolean(M, Env, NEnv,true)),
    evalBlock(N, NEnv,End).
evalIf(ifCondition(M,_N,O), Env,End):- 
    (evalCondition(M, Env, NEnv,false);evalBoolean(M, Env, NEnv,false)),
    evalBlock(O, NEnv,End).

%to evaluate the while loop
evalWhile(while(M,N), Env,End):- 
    evalBoolean(M, Env, NEnv,true),
    evalBlock(N, NEnv, NEnv1),
    evalWhile(while(M,N), NEnv1,End).
evalWhile(while(M,_N), Env, Env) :- 
    evalBoolean(M, Env, Env,false).
evalWhile(while(M,N), Env,End):- 
    evalCondition(M, Env, NEnv,true),
    evalBlock(N, NEnv, NEnv1),
    evalWhile(while(M,N), NEnv1,End).
evalWhile(while(M,_N), Env, Env) :- 
    evalCondition(M, Env, Env,false).

%to evaluate the for
evalFor(forLoop(M,N,O,W), Env,End):- 
    evalDeclare(M, Env, NEnv),
    loops(N,O,W, NEnv,End).
evalFor(forLoop(M,N,O,W), Env,End):- 
    evalAssignment(M, Env, NEnv),
    loops(N,O,W, NEnv,End).
loops(M,N,O, Env,End) :- 
    evalCondition(M, Env, Env,true),
    evalBlock(O, Env, NEnv),
    (evalIterator(N, NEnv, NEnv1);evalExpression(N, NEnv, NEnv1)),
    loops(M,N,O, NEnv1,End).
loops(M,_N,_O, Env, Env) :- 
    evalCondition(M, Env, Env,false).
loops(M,N,O, Env,End) :- 
    evalBoolean(M, Env, Env,true),
    evalBlock(O, Env, NEnv),
    (evalIterator(N, NEnv, NEnv1);evalExpression(N, NEnv, NEnv1)),
    loops(M,N,O, NEnv1,End).
loops(M,_N,_O, Env, Env) :- 
    evalBoolean(M, Env, Env,false).

%to evaluate the forInRange
evalForInRange(forLooprange(M,N,O,W), Env,End):- 
    evalCharTree(M,Id),
    ((evalValueTree(N, Val),update(int,Id, Val, Env, NEnv));
    (lookup(N, Env, Val),update(int,Id, Val, Env, NEnv))),
    ((evalValueTree(O,N));
    (evalCharTree(O,Id1),lookup(Id1, NEnv,N))),
    looping(Id,N,W, NEnv,End).
looping(M,O,W, Env,End):- 
    lookup(M, Env, Val),
    Val < O, 
    evalBlock(W, Env, NEnv),
    Val1 is Val + 1,
    update(int, M, Val1, NEnv, NEnv1),
    looping(M,O,W, NEnv1,End).
looping(M,O,_W, Env, Env) :- 
    lookup(M, Env, Val), 
    Val >= O.

%to evaluate ternary condition
evalTernary(ternary(M,N,_O), Env,End):- 
    (evalCondition(M, Env, NEnv,true);evalBoolean(M, Env, NEnv,true)),
    evalStatements(N, NEnv,End).
evalTernary(ternary(M,_N,O), Env,End):- 
    (evalCondition(M, Env, NEnv,false);evalBoolean(M, Env, NEnv,false)),
    evalStatements(O, NEnv,End).

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

%to evaluate addition,subtraction,multiplication and division
evalExpression(M, Env, NEnv) :- 
    evalAssignment(M, Env, NEnv).
evalExpression(M, Env, NEnv, Val) :- 
    evalExprHelper(M, Env, NEnv, Val).
evalExpression(sub(M,N), Env, NEnv, Val):-
    evalExpression(M, Env, Env1, Val1),
    evalExprHelper(N, Env1, NEnv, Val2),
    Val is Val1 - Val2.
evalExprHelper(M, Env, NEnv, Val) :- 
    evalExprHelper1(M, Env, NEnv, Val).
evalExprHelper(add(M,N), Env, NEnv, Val):-
    evalExprHelper(M, Env, Env1, Val1),
    evalExprHelper1(N, Env1, NEnv, Val2),
    Val is Val1 + Val2.
evalExprHelper1(M, Env, NEnv, Val) :- 
    evalExprHelper2(M, Env, NEnv, Val).
evalExprHelper1(mul(M,N), Env, NEnv, Val):-
    evalExprHelper1(M, Env, Env1, Val1),
    evalExprHelper2(N, Env1, NEnv, Val2),
    Val is Val1 * Val2.
evalExprHelper2(M, Env, NEnv, Val) :- 
    evalExprHelper3(M, Env, NEnv, Val).
evalExprHelper2(div(M,N),  Env, NEnv, Val):-
    evalExprHelper2(M, Env, Env1, Val1), 
    evalExprHelper3(N, Env1, NEnv, Val2),
    Val is floor(Val1 / Val2).
evalExprHelper3(M,  Env, NEnv, Val) :- 
    evalValue(M, Env, NEnv, Val).
evalExprHelper3(t_parentheses(M), Env, NEnv, Val):-
    evalExpression(M, Env, NEnv, Val).

%to evaluate the number and string
evalValue(value(Val), Env, Env, Val).
evalValue(identifier(I), Env, Env, Val) :-
    term_to_atom(Id,I),
    lookup(Id, Env, Val).
evalValueTree(value(Val), Val).
evalCharTree(identifier(I),Id):- 
    term_to_atom(Id,I).
evalStr(string(I), Env, Env, Val) :- 
    atom_string(I, Val).
