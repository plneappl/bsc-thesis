%% A copy of concret.pl with re-ordered inference rules
%% to achieve nontermination
%%
%% RHS grammar (abstract syntax):
%%
%% A := A + A  (A₁)
%% A := A × A  (A₃)
%% A := int    (A₅)
%%
%% LHS grammar (concrete syntax):
%%
%% C := C + S  (C₁)
%% C := S      (C₂)
%% S := S × F  (S₃)
%% S := F      (S₄)
%% F := int    (F₅)
%% F := ( C )  (F₆)
%%
%% Pattern synonyms:
%%
%% pattern C₂ x       = x :: A
%% pattern C₁ x Add y = A₁ x Add y
%% pattern S₄ x       = x :: A
%% pattern S₃ x Mul y = A₃ x Mul y
%% pattern F₆ LP x RP = x :: A
%% pattern F₅ n       = A₅ n
%%
%% Syntax trees are represented by nested predicates.
%% For example, the concrete syntax tree of "3 + 5" is
%%
%%   c1(c2(s4(f5(3))), add, s4(f5(5))),
%%
%% and the abstract syntax tree of "3 + 5" is
%%
%%   a1(a5(3), add, a5(5)).

c2a(c2(Xs), Xa) :-
  s2a(Xs, Xa).

c2a(c1(Xc, add, Ys), a1(Xa, add, Ya)) :-
  c2a(Xc, Xa),
  s2a(Ys, Ya).

s2a(s4(Xf), Xa) :-
  f2a(Xf, Xa).

s2a(s3(Xs, mul, Yf), a3(Xa, mul, Ya)) :-
  s2a(Xs, Xa),
  f2a(Yf, Ya).

f2a(f6(Xc), Xa) :-
  c2a(Xc, Xa).

f2a(f5(N), a5(N)).

%% | ?- c2a(X, a5(5)).
%% <loops forever>

%% | ?- c2a(X, a1(a3(a5(3), mul, a5(4)), add, a5(5))).
%% <loops forever>

%% | ?- c2a(c1(c2(s3(s4(f5(3)), mul, f5(4))), add, s4(f5(5))), Y).
%% Y = a1(a3(a5(3),mul,a5(4)),add,a5(5)) ?
%% yes
