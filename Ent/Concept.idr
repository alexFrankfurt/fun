module Ent.Concept

data EntCore : Type where
  Entity : EntCore
  Arrow : (a : EntCore) -> (b : EntCore) -> EntCore

syntax [ex1] "->" [ex2] = Arrow ex1 ex2

A : EntCore
A = Entity

B : EntCore
B = Entity

C : EntCore 
C = A -> B

D : EntCore
D = C -> A
