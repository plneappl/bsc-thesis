%% left recursion elimination as a system of inf rules
%%
%% RHS grammar (left recursive):
%%
%% S := S + F (S₁)
%% S := F     (S₂)
%% F == int
%%
%% RHS grammar (right recursive)
%%
%% A := F R    (A₁)
%% R  := + A   (R₁)
%% R  := ε     (R₂)
%%
%% Pattern synonyms:
%%
%% pattern A₁ f₁ r₂               =  S₂ f₁
%% pattern A₁ f₁ (S₂ f₂)          =  S₁ (S₂ f₁) Plus f₂
%% pattern A₁ f₁ (S₁ s Plus f₉)   =  S₁ (A₁ f₁ s) Plus f₉
%% pattern R₁ Plus s              =  s :: S

%% f1 is not defined; use integers for it.

relStoA(cS_2(F1), cA_1(F1, r2)).

relStoA(cS_1(cS_2(F1), plus, F2), cA_1(F1, R)) :- relRtoS(cS_2(F2), R).

relStoA(cS_1(S1, plus, Fn), cA_1(F1, R)) :-
  relStoA(S1, cA_1(F1, Sp)),
  relRtoS(S, Sp),
  relRtoS(cS_1(S, plus, Fn), R).

relRtoS(S, cS_1(plus, Sp)) :- relStoA(S, Sp).


%% tests

%% | ?- rel(a1(5,r2), s2(5)).
%% yes

%% | ?- rel(X, s2(5)).
%% X = a1(5,r2) ?
%% yes

%% | ?- rel(X, s1(s1(s2(1), plus, 2), plus, 3)).
%% X = a1(1,r1(plus,a1(2,r1(plus,a1(3,r2))))) ?
%% yes 

%% | ?- rel(a1(1,r1(plus,a1(2,r1(plus,a1(3,r2))))), Y).
%% Y = s1(s1(s2(1),plus,2),plus,3) ?
%% yes
