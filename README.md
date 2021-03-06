# Morpheus

Morpheus is a component bringing metamorphosing capabilities into Scala. The main goal of this component is to tackle with the design stiffness rooted in Scala's fusion of the strict type system and functional programming. These two concepts undeniably contribute to the developing of a robust code, however, the downside is that this approach often makes it difficult to model extensively mutable systems. Morpheus addresses this problem by introducing declarative and type-safe dynamics into Scala by means of a Scala compiler extension and a set of macros.

## Motivation

As the name suggests, Morpheus allows creating an object, which can assume various forms as a result of either external or internal events and adapt its behavior and state to a changeable environment.

The forms that the object can adopt are not arbitrary. Instead, all possible forms of the object are determined by a finite automaton model represented by so-called *morph type*. The morph type specifies two fundamentals: firstly the building blocks called *fragments* and secondly all possible combinations of fragments representing the object forms called *alternatives*. Morph types are expressed as Scala types and are checked in compile time so that the runtime part of Morpheus can be certain that all forms of an object under construction are valid and assembled correctly from the fragments. 

Allow me to demonstrate Morpheus in action in the following simple example, in which we will be modelling a simple chat application. Such an application inherently contains a lot of dynamics. One of the most obvious of which is the changing of a contact's status. So let’s begin with modelling the contact and its status.

First, in order to start using Morpheus, we have to include these two import statements:

```scala
import org.morpheus._
import org.morpheus.Morpheus._
```

The contact is the main entity in the application. It represents a remote user stored within the profile of the local user of the chat application. In contrast to the traditional OOP approach, in which an entity is represented by means of a class, here, we use traits for modelling entities. Incidentally, classes are second-class citizens in Morpheus and are only used as auxiliary types.

The contact comes in two basic flavors offline and online. It is natural to model these two flavors as two traits extending from the same parent trait `Contact` encapsulating common behaviour. This is the declaration the parent trait `Contact`:

```scala
trait Contact {
  def address: String
  def sendMessage(msg: String)
}
```

Let us assume at this stage that the common behavior is the sending of a message to the remote user. Additionaly, the common trait provides a textual representation of the address to which the message is sent. Each fragment keeps its own address and implements the sending procedure in its own way. The offline contact uses email as its communication channel, while the online contact uses the chat channel (which is under development).

Now we can declare the two traits representing the offline and online contacts. These traits become Morpheus fragments by annotating them by means of the `fragment` annotation. Each fragment trait has a special field for storing the proprietary address.

```scala
@fragment trait OfflineContact extends Contact {
  var emailAddress: String = _
  def address = emailAddress
  override def sendMessage(msg: String) {
    // todo
    println(s"Sending message to $emailAddress by email")
  }
}

@fragment trait OnlineContact extends Contact {
  var chatNick: String = _
  def address = chatNick
  override def sendMessage(msg: String) {
    // todo
    println(s"Sending message to $chatNick by chat")
  }
}
```

Now that we have completed the modelling phase, we can start to play with the model. Our next objective is to create a single object representing a chat contact capable of switching between the two alternative forms.

First, we have to declare the morph type, which specifies all possible alternatives and fragments constituting these alternatives. In this case the type declaration is quite straightforward since we simply express the fact that the resulting object can be either `OfflineContact` or `OnlineContact`:

```scala
  type ContactMorphType = OfflineContact or OnlineContact
```
The `or` 'keyword' is a special trait with two type parameters. Without the syntax flexibility possesed by Scala, which allows the writing of a type with two parameters in the in-fix way, the morph type would have to be written as `or[OfflineContact, OnlineContact]`.

In previous step we only declared a type alias. The static analysis of the morph type will take place in the following step in which we will create the so called *morph kernel*. The kernel is a structure whose purpose is to assemble a chosen alternative according to the *morph model* built from the morph type. The kernel also contains factories for all fragments specified in the morph type and knows the composition of all alternative forms that the mutating object can assume. The static analysis is done in a macro which analyses the morph type and checks a number of conditions. We will deal with these conditions in detail later on, but at this point we can reveal that one such condition is the dependency check (which we have yet to demonstrate), which verifies that all dependencies of all fragments in all alternative forms are satisfied.

*Note: Although fragment instances used by the kernel are real objects, they are not meant to be used outside of the scope of an alternative form.*

The `singleton` macro creates a morph kernel that uses singleton *fragment factorie* for creating fragments. It causes that all alternative forms produced by the kernel will de-facto reuse the fragment instances provided that the singleton factories create fragment instances just once.

```scala
    val contactKernel = singleton[ContactMorphType]
```

Besides the singleton macro there is also the `compose` macro using pure (non-singleton ) fragment factories. In this case every alternative would be composed of fresh fragment instances.

Now we are ready to create our first mutating object, i.e. a morph in Morpheus terminology. Before that we have to explain how the kernel knows which alternative form it is to assemble. The kernel cooperates with so called *morphing strategy* that iterates through all alternative forms and picks the winner. The morphing strategy is a customisable component, however, there is a couple of out of the box strategies. Moreover, the kernel has the default morphing strategy used when the strategy is not explicitly specified. The *default default strategy* picks the left-most alternative as the winner. We will explain what *left-most* exactly means later, but in this example it is obvious that it leads to picking the alternative consisting of the `Offline` fragment only.

The kernel contains the special lazy member `~` holding the default morph, i.e. the morph adopting initially the alternative form chosen by the default strategy. Morphs can be directly accessed as if they were instances of the *lowest upper bound* type (LUB) of all alternatives in the morph model. In this case the LUB of the morph model is `Contact`.

```scala
  val contact: Contact = contactKernel.~
  println(contact.address) // accessing a common member
```

Since we know that the default strategy picked the `Offline` alternative, we may be tempted to typecast the morph to the `Offline` type or use the match keyword to do it more safely:
    
```scala
  contact match {
     case off: OfflineContact => //
     case on: OnlineContact => //
  }
```  
      
Unfortunately, it does not work that straightforwardly. The morph reference is in fact a proxy instance implementing the LUB only regardless the alternative form adopted by the morph.
It raises the question how we can access the current morph alternative. The select macro is the answer. It takes the type argument specifying the type against which we want to test the morph’s current alternative. The second arguments is the morph. The result of this macro is `Option[T]`, where `T` is the type used as the argument.

```scala
    select[OfflineContact](contactKernel.~) match {
    case None => sys.error("unexpected alternative")
    case Some(offlineContact) => offlineContact.emailAddress = args(0)
    }
```

The last case branch gives direct access to `OfflineContact`, thus we can initialise the email address member by the first application argument.

The select macro does not only checks the type of the alternative against the argument. It also determines in compile time whether the argument type is valid with respect to the morph model. In other words it checks whether there is an alternative in the model that could be assigned to the type argument.
For example, the following statement will not compile since there is no alternative assignable to `String`.

```scala    
  // select[String](contactKernel.~) // it does not compile since String is not a valid alternative
```

Now another question must come to mind. How can we change the current alternative? Well, there are several ways to do it. However because of the limited scope of this introduction we will deal with the simplest one.

Besides the LUB of the morph model, every morph object also implements `org.morpheus.MutableMorphMirror` trait. This trait contains a couple of methods providing reflective access to the morph and its active alternative form. One of such methods is `remorph(MorphingStrategy)`. This method is used for changing the underlying alternative form of the morph with the help of a given morphing strategy passed as the method argument. So before we invoke the remorph method we have to create a morphing strategy.

Every morphing strategy implements `org.morpheus.MorphingStrategy` trait.  Although it is possible and quite easy to implement this trait directly, there is a couple of predefined handy strategies. Here, we will use the *promoting* strategy. As the name suggests this strategy promotes the alternative explicitly selected by passing its ordinal number as the argument. This strategy is created by macro `promote`. There are two overloaded versions of this macro and we are using one where the type argument specifies a compatible sub-model type of the morph model and the other argument takes a function returning the ordinal number selecting an alternative from the sub-model. In this case we use the original morph model type as the type argument, which is compatible by definition, and as the function selecting the alternative we pass the `contactAlt` variable, which, thanks to some implicit conversions, gets converted to a parameterless function returning Int. Variable `contactAlt` is initialised to `1` indicating that we are interested in the second alternative, which is `OnlineContact`.

```scala
    var contactAlt = 1
    val contactMorphStrategy = promote[ContactMorphType](contactAlt)
```

Now we can finally pass the strategy to the `remorph` method.

```scala
  contactKernel.~.remorph(contactMorphStrategy)
```

We expect that the morph is now adopting the alternative with the `OnlineContact` fragment, what we can verify by the following statement and use it for initialisation of member `chatNick`.

```scala
  select[OnlineContact](contactKernel.~) match {
    case None => sys.error("unexpected alternative")
    case Some(onlineContact) => onlineContact.chatNick = args(1)
  }
```

Since we have initialised all members, we can use the common `sendMessage` method regardless the alternative form the morph is taking on.

First, we are going to activate the `OfflineContact` again, what brings us to another method from `MutableMorphMirror`. The no-args `remorph` method causes re-morphing of the morph by means of the strategy used to assemble the last alternative. Before we call it, we have to change the value of `contactAlt` to `0` indicating the first alternative.

```scala
  contactAlt = 0
  contactKernel.~.remorph
```

Now let’s invoke the two common methods `address` and `sendMessage`.

```scala
  println(s"Recipient address ${contactKernel.~.address}")
  contactKernel.~.sendMessage("Hello!")
```

The console should read:

```
Recipient address pepa@gmail.com
Sending message to pepa@gmail.com by email 
```

As the last step of this brief tutorial switch the morph to the second alternative.

```scala
  contactAlt = 1
  contactKernel.~.remorph
  println(s"Recipient address ${contactKernel.~.address}")
  contactKernel.~.sendMessage("Hello again!")
```

Now the console should read this:

```
Recipient address pepa
Sending message to pepa by chat
```

## Quick Start

Assuming git, sbt and mvn installed:

```bash
 host$ git clone https://github.com/zslajchrt/morpheus.git
 host$ cd morpheus
 host$ sbt package publishM2
 host$ cd ..
 host$ git clone https://github.com/zslajchrt/morpheus-tutor.git
 host$ cd morpheus-tutor
 host$ mvn package

```

## Find out more

* [Wiki Home](https://github.com/zslajchrt/morpheus/wiki/Home)
* [IDE Setup](https://github.com/zslajchrt/morpheus/wiki/IDE_Setup)
* [Tutorial](https://github.com/zslajchrt/morpheus-tutor)
* [Features Tests](https://github.com/zslajchrt/morpheus-tests)

## License

Licensed under the **[Apache License, Version 2.0](LICENSE)** (the "License");
you may not use this software except in compliance with the License.

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
