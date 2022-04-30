venom(Lexername, Filename) :-
    term_to_atom(Y, X),
    program(Tree, Y, []),
    evalProgram(Tree, Output).

:- table boolean/3, expression/3, term/3.

% Parsing the program 
program(Program(M)) -->['begin'], block(M), ['end'].

% Parsing the block
block(Block(M)) --> ['{'], blockSection(M), ['}']. 
blockSection(Block(M)) --> statements(M).
blockSection(Block(M, N)) --> statements(M), blockSection(N).

% Parsing statements 
statements(Statements(X)) --> 
    declaration(X), [;].
statements(Statements(X)) --> 
    assignment(X), [;].
statements(Statements(X)) --> 
    expression(X), [;].
statements(Statements(X)) --> 
    boolean(X), [;].
statements(Statements(X)) --> 
    printstatements(X), [;].
statements(Statements(X)) --> 
    ifcondition(X).
statements(Statements(X)) --> 
    ternarycondition(X), [;].
statements(Statements(X)) --> 
    forloop(X).
statements(Statements(X)) --> 
    whileloop(X).
statements(Statements(X)) --> 
    forrange(X).
statements(Statements(X)) --> 
    iterator(X), [;].

% Parsing for loop
forLoop(ForLoop(M, N, O, P)) --> 
    ['for'], ['['], declaration(M), [';'], (condition(N);boolean(N)), [';'], iterator(O), [']'], block(P).
forLoop(ForLoop(M, N, O, P)) --> 
    ['for'], ['['], declaration(M), [';'], (condition(N);boolean(N)), [';'], assignment(O), [']'], block(P).
forLoop(ForLoop(M, N, O, P)) --> 
    ['for'], ['['], assignment(M), [';'], (condition(N);boolean(N)), [';'], iterator(O), [']'], block(P).
forLoop(ForLoop(M, N, O, P)) --> 
    ['for'], ['['], assignment(M), [';'], (condition(N);boolean(N)), [';'], expression(O), [']'], block(P).

% Parsing forRange loop
forRange(ForRange(M, N, O, P)) --> 
    ['for'], identifier(M), ['in'], ['range'], ['['], num(N), ['--'], num(O), [']'], block(P).
forRange(ForRange(M, N, O, P)) --> 
    ['for'], identifier(M), ['in'], ['range'], ['['], identifier(N), ['--'], identifier(O), [']'], block(P).
forRange(ForRange(M, N, O, P)) --> 
    ['for'], identifier(M), ['in'], ['range'], ['['], num(N), ['--'], identifier(O), [']'], block(P).
forRange(ForRange(M, N, O, P)) --> 
    ['for'], identifier(M), ['in'], ['range'], ['['], identifier(N), ['--'], num(O), [']'], block(P).

% Parsing M while loop
whileloop(WhileLoop(M, N)) --> 
    ['while'], ['('], (condition(M);boolean(M)), [')'], block(N).

% Parsing boolean expressions
boolean(true) --> ['true'].
boolean(false) --> ['false'].
boolean(Bool_Not(X)) --> ['not'],['('], boolean(X), [')'].
boolean(Bool_Not(X)) --> ['not'],['('], condition(X), [')'].
boolean(Bool_And(X, Y)) --> boolean(X), ['and'], boolean(Y).
boolean(Bool_And(X, Y)) --> condition(X), ['and'], condition(Y).
boolean(Bool_Or(X, Y)) --> boolean(X), ['or'], boolean(Y).
boolean(Bool_Or(X, Y)) --> condition(X), ['or'], condition(Y).

not(true, false).
not(false, true).

and(false, _, false).
and(_, false, false).
and(true, true, true).

or(true, _, true).
or(_, true, true).
or(false, false, false).

%Evaluations begin here

%to evaluate the program
evalProgram(t_Program(X), FinalEnv) :- 
    evalBlock(X, [], FinalEnv), !.

%to evaluate the block
evalBlock(t_Block(X), Env, FinalEnv) :- 
    eval_block_section(X, Env, FinalEnv).
eval_block_section(t_Block(X, Y), Env, FinalEnv) :- 
    eval_statements(X, Env, Env1),  eval_block_section(Y, Env1, FinalEnv).
eval_block_section(t_Block(X), Env, FinalEnv) :- eval_statements(X, Env, FinalEnv).

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
    eval_declare(X, Env, NewEnv),
    loops(Y,Z,W, NewEnv,FinalEnv).
evalForloop(t_ForLoop(X,Y,Z,W), Env,FinalEnv):- 
    eval_assign(X, Env, NewEnv),
    loops(Y,Z,W, NewEnv,FinalEnv).
loops(X,Y,Z, Env,FinalEnv) :- 
    evalCondition(X, Env, Env,true),
    evalBlock(Z, Env, NewEnv),
    (evalIter(Y, NewEnv, NewEnv1);eval_expr(Y, NewEnv, NewEnv1)),
    loops(X,Y,Z, NewEnv1,FinalEnv).
loops(X,_Y,_Z, Env, Env) :- 
    evalCondition(X, Env, Env,false).
loops(X,Y,Z, Env,FinalEnv) :- 
    evalBoolean(X, Env, Env,true),
    evalBlock(Z, Env, NewEnv),
    (evalIter(Y, NewEnv, NewEnv1);eval_expr(Y, NewEnv, NewEnv1)),
    loops(X,Y,Z, NewEnv1,FinalEnv).
loops(X,_Y,_Z, Env, Env) :- 
    evalBoolean(X, Env, Env,false).

%to evaluate the forRange
evalForrange(t_ForRange(X,Y,Z,W), Env,FinalEnv):- 
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