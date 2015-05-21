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
%% S' := F R   (S₁')
%% R  := + S'  (R₁)
%% R  := ε     (R₂)
%%
%% Pattern synonyms:
%%
%% pattern S₁' f₁ r₂              =  S₂ f₁
%% pattern S₁' f₁ (S₂ f₂)         =  S₁ (S₂ f₁) Plus f₂
%% pattern S₁' f₁ (S₁ s Plus f₉)  =  S₁ (S₁' f₁ s) Plus f₉
%% pattern R₁ Plus s              =  s :: S

%% f1 is not defined; use integers for it.

rel(s1p(F1, r2), s2(F1)).

rel(s1p(F1, R), s1(s2(F1), plus, F2)) :- relR(R, s2(F2)).

rel(s1p(F1, R), s1(S1, plus, Fn)) :-
  rel(s1p(F1, Sp), S1),
  relR(Sp, S),
  relR(R, s1(S, plus, Fn)).

relR(r1(plus, Sp), S) :- rel(Sp, S).


%% tests

%% | ?- rel(s1p(5,r2), s2(5)).
%% yes

%% | ?- rel(X, s2(5)).
%% X = s1p(5,r2) ?
%% yes

%% | ?- rel(X, s1(s1(s2(1), plus, 2), plus, 3)).
%% X = s1p(1,r1(plus,s1p(2,r1(plus,s1p(3,r2))))) ?
%% yes

%% | ?- rel(s1p(1,r1(plus,s1p(2,r1(plus,s1p(3,r2))))), Y).
%% Y = s1(s1(s2(1),plus,2),plus,3) ?
%% yes
