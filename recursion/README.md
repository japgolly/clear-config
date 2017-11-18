# Recursion Scheme Cheatsheet

## Basic

| Name | Type | Desc |
|--|--|--|
| cata | `(f a -> a) -> Fix f -> a` | Fold |
| ana  | `(a -> f a) -> a -> Fix f` | Unfold |
| hylo | `(a -> f a) -> (f b -> b) -> a -> b` | Efficiently unfold & fold |

## Promorphisms

Transform (expand/prune/update) subtrees during morphism.

| Name | Type | Desc |
|--|--|--|
| prepro | `(f ~> f) -> (f a -> a) -> Fix f -> a` | cata that transforms children before folding. <br> Top-most structure (i.e. the input) is not transformed. <br> Outside to inside. |
| postpro  | `(f ~> f) -> (a -> f a) -> a -> Fix f` | ana that creates a structure, transforming each new child<br>(i.e. the entire structure as exists at the end of a pass). <br> Top-most structure (i.e. the end result) is not transformed. <br> Inside to outside. |

## Elgot

Hylos that can short-circuit (in terms of recursion depth).

| Name | Type | Desc |
|--|--|--|
| elgot | `(a -> b \/ f a) -> (f b -> b) -> a -> b` | short-circuit during ana  |
| coelgot | `(a -> f a) -> ((a, () -> f b) -> b) -> a -> b` | short-circuit during cata  |
