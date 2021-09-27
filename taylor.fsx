let f x = System.Math.Atan(x)
let rec fix func x = func (fix func) x
let a = 0.0
let b = 0.5
let eps = 0.0000001
let m = 10

let naive_impl =
    fix(
        fun func acc n x ->
            let add = (-1.)**(n) * x**(2.*n+1.) / (2.*n+1.)
            if abs(add) < eps then acc, int(n)
            else func (acc + add) (n + 1.) x
    ) 0. 1.

let efficient_impl x =
    fix(
        fun func acc n x prev prev_pow ->
            let next = prev *(-1.)/2.
            let add = next * x * prev_pow
            if abs(add) >= eps then func (acc + add) (n + 1.) x next (x * prev_pow)
            else acc, int(n)
    ) x 1. x 1. x

let main =
   for j=0 to m do
     let y = a+(float j)/(b-a)*(float m)
     let efficient, efficient_ = efficient_impl y
     let naive, naive_ = naive_impl y
     printfn "%5.2f  %10.6f  %10.6f   %2d   %10.6f   %2d" y (f y) naive naive_ efficient efficient_

main
