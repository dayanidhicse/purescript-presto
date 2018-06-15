# Design of the new EDSLs

## Problem

Originally, the Flow Free monad is designed as one big monad. It has functionalities like calling API, store/load values in cache, run UI operations, flow management, etc. It's intended to allow the developers to do all those different operations in the Flow monad context. But this design has the problem that it's just the Free monad version of doing everything in Eff/Aff. Without seperation of concern, user is allowed to do anything he/she wants in a Flow function. And the great type system is marginalized not to provide much help on catching bugs.

## Solutions

The first problem to tackle is seperating the different domains of operations into their own EDSL. So all UI operations are groupped into a GuiF algebra data type, cache operations into their own ADT, etc. While flow operations like fork/await/launch/doAff are in the new Flow monad. Then we have to make it easy that users can compose different EDSL operations in one function and let type system to prevent any unintended function calls. Thankfully, we can compose Free monad easily using Coproduct and a Inject class.

The Inject class and instances are defined like this:

```
class Inject f g where
    inj :: forall a. f a -> g a
    prj :: forall a. g a -> Maybe (f a)

instance injectIdentity :: Inject f f where
    inj = id
    prj = Just

instance injectLeftCoproduct :: Inject f (Coproduct f g) where
    inj = Coproduct <<< Left
    prj = coproduct Just (const Nothing)

instance injectRightCoproduct :: Inject f g => Inject f (Coproduct h g) where
    inj = Coproduct <<< Right <<< inj
    prj = coproduct (const Nothing) prj
```
"Inject f g" means any operation in f can be supported in g.

So users can write functions like this:
```
showUIDemo :: forall f a. Inject GuiF f => a -> Free f Unit
showUIDemo a = ...

callRest :: forall f a b. Inject ApiF f => a -> Free f b
callRest req = ...
```
In the showUIDemo function, it's specifically stated that f support alll GuiF operations, but not others. So we can call any GuiF defined operations. But you can't call an API or load a cached value inside. Similarly, in callRest function you can only call remote APIs but can't show an UI.

Then, what should we do if we want to call an remote API and show the response data in UI? We need to compose them like this:
```
showRestResult :: forall f. Inject GuiF f => Inject ApiF f => Free f Unit
showRestResult = do
    res <- callRest ...
    showUIDemo res
```
In this function, the two Inject instance specified that we can run GuiF and ApiF operations, but not others. And the actual code is very straightforward too. It's so easy to compose that it looks like they're in the same EDSL. But the type system insured that we can only do UI and API stuff in it and can't touch the cache.

So this solution is elegant and safe.

## Interpretation
With the languages defined, we still need to interpret those EDSLs into Eff/Aff monad so it can actually run. There're several ways of doing it. One popular thought is composing Free EDSL using Coproduct while composing interpreters using Product of Cofree monad. The general idea is defining different interpreters for the EDSLs in Cofree monad and compose them using Product. It's elegant and works great for pure languages. But if we want to run effectful actions in an interpreter, the effect will need to be exposed in the language design, which is not elegant and hard to use. Check out [Dave Liang's posts](http://dlaing.org/cofun/) on this topic.

At the end, based on the wonderful paper [Data types à la carte](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.101.4131), I've come up with a pretty good interpreter framework.

First, I defined the Run class for interpreters between one EDSL and one specific Monad.
```
class Run f m where
    runAlgebra :: forall a. f a -> m a

instance runCoproduct :: (Run f m, Run g m) => Run (Coproduct f g) m where
    runAlgebra (Coproduct e) = either runAlgebra runAlgebra e

run :: forall f m a. Run f m => MonadRec m => Free f a -> m a
run = foldFree runAlgebra
```
Here, "Run f m" means I can interpret "f a" into "m a". As you can see, if we can interpret f to m and g to m, then we can interpret the Coproduct f g into m. With the help of foldFree function, "run" is defined to actually interpret a Free monad into result m monad.

Now we can define interpreters for those EDSLs like GuiF, ApiF. But what context monad should we interpret them to? We can define instances like "Run GuiF (Aff e)" to interpret GuiF into Aff. But there're three problems here.

* We want user to fork/await/launch flows, and operations like GuiF/ApiF should be possible inside that flow.
* New Flow language can't be interpreted with the Run class due to recursive definition. 
* We want user to provide runtime info like UIRunner, APIRunner for our interpreter. But we can only define static interpreters with Run.

The final solution is defining instances like "Run GuiF Flow", "Run ApiF Flow" to interpret those EDSLs into Flow monad. The Flow monad will have basic public operations like fork, launch, but also private operations to support GuiF/ApiF etc. At last, we defined function "runFlow" to let user interpret the Flow to InterpreterSt monad with provided Runtime.

## Conclusion
So, the general design is seperating GuiF, ApiF, StoreF and PermissionF EDSLs, composing them with Coproduct and interpret them into Flow. Then control flow operations like fork/await/launch can be applied. At last, runFlow will interpret Flow into Aff. Checkout the example projects on how they're used.

## Other goodies
In order to introduce an universal data type in EDSL definition, the Exists type in Data.Exists module is used in the original Flow definition.
```
data Exists :: (Type -> Type) -> Type
```
But it also make the code more complex and hard to define instances, as we use data types like "FlowMethod a s" and "Exists (FlowMethod a)" to hide the s inside. We can't define instances for "FlowMethod s" over type variable a.

Under the hood, it's just unsafeCoerce data types to fool the type system. In order to make it easy for us to use, I've defined a similar Existing data type:
```
data Existing :: (Type -> Type -> Type) -> Type -> Type
```
Then we can define data like "FlowMethod s a" and define functor instance for "FlowMethod s" and hide the s with "Existing FlowMethod a". This trick simplifies the code and make some impossible things possible.
