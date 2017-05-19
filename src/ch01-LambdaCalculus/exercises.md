### Alpha equivalence

`λx.x ~ λy.y ~ λz.z`

### Beta reduction

`(λx.x) 2 = 2`
`(λx.x)(λy.y) = λy.y`

### Free variables

`λx.xy  here x is bound variable, y is free variable`

### Multiple arguments

`λxy.xy ~ λx.(λy.xy)   curring...`

### Evaluation

```
(λxyz.xz(yz))(λmn.m)(λp.p)
= (λx.(λy.(λz.xz(yz))))(λmn.m)(λp.p)
= (λy.(λz.(λmn.m)z(yz)))(λp.p)
= (λz.(λmn.m)(z)((λp.p)(z))
= (λz.(λmn.m)zz)
= (λz.z)
= λz.z
```

### equivalence

` λxy.xz ~ λmn.mz`
`λxy.xxy ~ λa.(λb.aab)`
`λxyz.zx ~ λtos.st`


### Combinators: a lambda term with no free variables

#### Yes:
`λx.x`
`λxy.x`
`λxyz.xz(yz)`
### No:
`λy.x`
`λx.xz`


### Chapter exercises: Beta reduce

```
1. (λabc.cba)zz(λwv.w)
=  (λwv.w)zz
=  z

2. (λx.λy.xyy)(λa.a)b
=  (λa.a)bb
=  bb

3. (λy.y)(λx.xx)(λz.zq)
=  (λx.xx)(λz.zq)
=  (λz.zq)(λz.zq)
=  (λz.zq)q
=  qq

4. (λz.z)(λz.zz)(λz.zy)
=  (λz.zz)(λz.zy)
=  (λz.zy)(λz.zy)
=  (λz.zy)y
=  yy

5. (λx.λy.xyy)(λy.y)y
=  (λz.z)yy
=  yy

6. (λa.aa)(λb.ba)c
=  (λb.ba)(λb.ba)c
=  (λb.ba)ac
=  aac

7. (λxyz.xz(yz))(λx.z)(λx.a)
=  (λmns.ms(ns))(λx.z)(λx.a)
=  (λs.(λx.z)s((λx.a)s)
=  (λs.za)
=  λs.za
```
