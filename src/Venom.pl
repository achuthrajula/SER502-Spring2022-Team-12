venom(Lexername, Filename) :-
    term_to_atom(Y, X),
    program(Tree, Y, []),
    evalProgram(Tree, Output).

:- table boolean/3, expression/3, term/3.

% Parsing the program 
program(t_Program(M)) -->['begin'], block(M), ['end'].

% Parsing the block
block(t_Block(M)) --> ['{'], blockSection(M), ['}']. 
blockSection(t_Block(M)) --> statements(M).
blockSection(t_Block(M, N)) --> statements(M), blockSection(N).

% Parsing for loop
forLoop(t_ForLoop(M, N, O, P)) --> ['for'], ['['], declaration(M), [';'], (condition(N);boolean(N)), [';'], iterator(O), [']'], block(P).
forLoop(t_ForLoop(M, N, O, P)) --> ['for'], ['['], declaration(M), [';'], (condition(N);boolean(N)), [';'], assignment(O), [']'], block(P).
forLoop(t_ForLoop(M, N, O, P)) --> ['for'], ['['], assignment(M), [';'], (condition(N);boolean(N)), [';'], iterator(O), [']'], block(P).
forLoop(t_ForLoop(M, N, O, P)) --> ['for'], ['['], assignment(M), [';'], (condition(N);boolean(N)), [';'], expression(O), [']'], block(P).

% Parsing forRange loop
forRange(t_ForRange(M, N, O, P)) --> ['for'], identifier(M), ['in'], ['range'], ['['], num(N), ['--'], num(O), [']'], block(P).
forRange(t_ForRange(M, N, O, P)) --> ['for'], identifier(M), ['in'], ['range'], ['['], identifier(N), ['--'], identifier(O), [']'], block(P).
forRange(t_ForRange(M, N, O, P)) --> ['for'], identifier(M), ['in'], ['range'], ['['], num(N), ['--'], identifier(O), [']'], block(P).
forRange(t_ForRange(M, N, O, P)) --> ['for'], identifier(M), ['in'], ['range'], ['['], identifier(N), ['--'], num(O), [']'], block(P).

% Parsing M while loop
whileloop(t_WhileLoop(M, N)) --> ['while'], ['('], (condition(M);boolean(M)), [')'], block(N).
% Parsing for Print statement
flash(f_print(X)) --> ['print'], identifier(X).
flash(f_print(X)) --> ['print'], num(X).
flash(f_print(X)) --> ['print'], string(X).

%Evaluations begin here

%to evaluate the program
evalProgram(t_Program(X), FinalEnv) :- evalBlock(X, [], FinalEnv), !.

%to evaluate the block
evalBlock(t_Block(X), Env, FinalEnv) :- eval_block_section(X, Env, FinalEnv).
eval_block_section(t_Block(X, Y), Env, FinalEnv) :- eval_statements(X, Env, Env1), 
    eval_block_section(Y, Env1, FinalEnv).
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
 %to evaluating print statements
evaluate_flash(f_print(X), Env, Env) :- 
    evaluate_character_tree(X,Id),
    lup(Id, Env, Value),
    writeln(Value).
evaluate_flash(f_print(X), Env, Env) :- 
    eval_numtree(X, Value),
    writeln(Value).
evaluate_flash(f_print(X), Env, Env) :- 
    eval_str(X, Env, Env, Value),
    writeln(Value).  