| package |
package := Package name: 'jpp16'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #C;
	add: #Clause;
	add: #LogicalItem;
	add: #Pair;
	add: #Prolog;
	add: #Rule;
	add: #Term;
	add: #V;
	yourself.

package globalNames
	add: #L;
	yourself.

package binaryGlobalNames: (Set new
	add: #L;
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #LogicalItem
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Prolog
	instanceVariableNames: 'facts rules'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LogicalItem subclass: #Clause
	instanceVariableNames: 'first rest'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LogicalItem subclass: #Rule
	instanceVariableNames: 'head body'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LogicalItem subclass: #Term
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Term subclass: #C
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Term subclass: #Pair
	instanceVariableNames: 'first second'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Term subclass: #V
	instanceVariableNames: 'term varName dependentVars'
	classVariableNames: 'Vars'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

LogicalItem guid: (GUID fromString: '{F191C0A1-76B4-4678-904B-810909F7E47F}')!
LogicalItem comment: ''!
!LogicalItem categoriesForClass!Kernel-Objects! !
!LogicalItem methodsFor!

& rest
	^Clause first: self rest: rest!

interpreter: p go: term do: action
	self subclassResponsibility!

isBound
	self subclassResponsibility! !
!LogicalItem categoriesFor: #&!public! !
!LogicalItem categoriesFor: #interpreter:go:do:!public! !
!LogicalItem categoriesFor: #isBound!public! !

Prolog guid: (GUID fromString: '{F5BABEA8-918E-4A68-9AF3-6BE936D87D6B}')!
Prolog comment: ''!
!Prolog categoriesForClass!Kernel-Objects! !
!Prolog methodsFor!

fact: t
	facts add: t!

go: term do: action
	facts do: [ :fact |
		term interpreter: self go: fact do: action
	].
	rules do: [ :rule |
		rule interpreter: self go: term do: action
	].!

head: h body: b
	rules add: (Rule head: h body: b).!

initialize
	facts := OrderedCollection new.
	rules := OrderedCollection new.!

printOn: aStream
	aStream
		nextPut: $P;
		nextPut: $(;
		print: facts;
		nextPut: $,;
		space;
		print: rules;
		nextPut: $).
		! !
!Prolog categoriesFor: #fact:!public! !
!Prolog categoriesFor: #go:do:!public! !
!Prolog categoriesFor: #head:body:!public! !
!Prolog categoriesFor: #initialize!public! !
!Prolog categoriesFor: #printOn:!public! !

!Prolog class methodsFor!

new
	^super new initialize! !
!Prolog class categoriesFor: #new!public! !

Clause guid: (GUID fromString: '{0EE0BC45-445C-454D-9FF6-5209DFDF9718}')!
Clause comment: ''!
!Clause categoriesForClass!Kernel-Objects! !
!Clause methodsFor!

first: t rest: h
	first := t.
	rest := h.!

interpreter: p go: term do: action
	first interpreter: p go: term do: [ 
		p go: rest do: action.
	].!

isBound
	^first isBound & rest isBound!

printOn: aStream
	aStream
		print: first;
		space;
		nextPut: $&;
		space;
		print: rest.! !
!Clause categoriesFor: #first:rest:!public! !
!Clause categoriesFor: #interpreter:go:do:!public! !
!Clause categoriesFor: #isBound!public! !
!Clause categoriesFor: #printOn:!public! !

!Clause class methodsFor!

first: h rest: t
	^self new first: h rest: t! !
!Clause class categoriesFor: #first:rest:!public! !

Rule guid: (GUID fromString: '{85D31ECF-FE43-455F-ABBB-789990A43323}')!
Rule comment: ''!
!Rule categoriesForClass!Kernel-Objects! !
!Rule methodsFor!

body
	^body.!

head
	^head.!

head: h body: b
	head := h.
	body := b.!

interpreter: p go: term do: action
	head interpreter: p go: term do: [
		body interpreter: p go: head do: action
	].!

isBound
	^head isBound & body isBound!

printOn: aStream
	aStream
		print: head;
		space;
		nextPut: $:;
		nextPut: $-;
		space;
		print: body.! !
!Rule categoriesFor: #body!public! !
!Rule categoriesFor: #head!public! !
!Rule categoriesFor: #head:body:!public! !
!Rule categoriesFor: #interpreter:go:do:!public! !
!Rule categoriesFor: #isBound!public! !
!Rule categoriesFor: #printOn:!public! !

!Rule class methodsFor!

head: h body: b
	^self new head: h body: b! !
!Rule class categoriesFor: #head:body:!public! !

Term guid: (GUID fromString: '{2D30A7A8-31B7-4712-8FD8-B58AE27D4AF8}')!
Term comment: ''!
!Term categoriesForClass!Kernel-Objects! !
!Term methodsFor!

% v
	^Pair first: self second: (C % v)!

, right
	^Pair first: self second: right!

@ n
	^Pair first: self second: (V @ n)!

go: other do: action
	| dict |
	dict := V saveDict.
	(self unify: other) ifTrue: [
		action value
	].
	V restoreDict: dict.
	self restoreVars.
	other restoreVars.!

interpreter: p go: other do: action
	self go: other do: action!

isBound
	self subclassResponsibility!

restoreVars
	self subclassResponsibility!

unify: other
	self subclassResponsibility!

unifyPair: pair
	self subclassResponsibility!

unifyValue: val
	self subclassResponsibility!

unifyVariable: var
	self subclassResponsibility!

value
	self subclassResponsibility!

varInside: varName
	self subclassResponsibility! !
!Term categoriesFor: #%!public! !
!Term categoriesFor: #,!public! !
!Term categoriesFor: #@!public! !
!Term categoriesFor: #go:do:!public! !
!Term categoriesFor: #interpreter:go:do:!public! !
!Term categoriesFor: #isBound!public! !
!Term categoriesFor: #restoreVars!public! !
!Term categoriesFor: #unify:!public! !
!Term categoriesFor: #unifyPair:!public! !
!Term categoriesFor: #unifyValue:!public! !
!Term categoriesFor: #unifyVariable:!public! !
!Term categoriesFor: #value!public! !
!Term categoriesFor: #varInside:!public! !

C guid: (GUID fromString: '{328C54B7-C96D-472F-86A6-89BC75CEF3D6}')!
C comment: ''!
!C categoriesForClass!Kernel-Objects! !
!C methodsFor!

isBound
	^true!

printOn: aStream
	aStream
		nextPut: $C;
		nextPut: $(;
		print: value;
		nextPut: $).!

restoreVars!

unify: other
	^other unifyValue: self!

unifyPair: pair
	^false!

unifyValue: val
	^(self value) = (val value)!

unifyVariable: var
	^var unifyValue: self!

value
	^value!

value: v
	value := v!

varInside: varName
	^false! !
!C categoriesFor: #isBound!public! !
!C categoriesFor: #printOn:!public! !
!C categoriesFor: #restoreVars!public! !
!C categoriesFor: #unify:!public! !
!C categoriesFor: #unifyPair:!public! !
!C categoriesFor: #unifyValue:!public! !
!C categoriesFor: #unifyVariable:!public! !
!C categoriesFor: #value!public! !
!C categoriesFor: #value:!public! !
!C categoriesFor: #varInside:!public! !

!C class methodsFor!

% value
	^self new value: value! !
!C class categoriesFor: #%!public! !

Pair guid: (GUID fromString: '{92A587A9-ADE3-4F0C-95EC-7CDE89AF7844}')!
Pair comment: ''!
!Pair categoriesForClass!Kernel-Objects! !
!Pair methodsFor!

car
	^first!

cdr
	^second!

first: f second: s
	first := f.
	second := s!

isBound
	^first isBound & second isBound.!

printOn: aStream
	aStream
		nextPut: $<;
		print: first;
		nextPut: $,;
		space;
		print: second;
		nextPut: $>.!

restoreVars
	self car restoreVars.
	self cdr restoreVars.!

unify: other
	^other unifyPair: self!

unifyPair: pair
	| left right |
	left := (self car) unify: (pair car).
	right := (self cdr) unify: (pair cdr).
	^ left & right!

unifyValue: val
	^false!

unifyVariable: var
	^var unifyPair: self!

value
	^Pair first: (self car value) second: (self cdr value) !

varInside: varName
	| left right |
	left := self car varInside: varName.
	right := self cdr varInside: varName.
	^ left | right! !
!Pair categoriesFor: #car!public! !
!Pair categoriesFor: #cdr!public! !
!Pair categoriesFor: #first:second:!public! !
!Pair categoriesFor: #isBound!public! !
!Pair categoriesFor: #printOn:!public! !
!Pair categoriesFor: #restoreVars!public! !
!Pair categoriesFor: #unify:!public! !
!Pair categoriesFor: #unifyPair:!public! !
!Pair categoriesFor: #unifyValue:!public! !
!Pair categoriesFor: #unifyVariable:!public! !
!Pair categoriesFor: #value!public! !
!Pair categoriesFor: #varInside:!public! !

!Pair class methodsFor!

first: f second: s
	^self new first: f second: s! !
!Pair class categoriesFor: #first:second:!public! !

V guid: (GUID fromString: '{BFF425ED-73E8-4520-B3B1-0A744C6E2709}')!
V comment: ''!
!V categoriesForClass!Kernel-Objects! !
!V methodsFor!

addDependent: v
	dependentVars add: v!

assertBound
	self term ifNil: [
		self error: 'Variable is unbound'.
	].!

car
	self assertBound.
	^self term car!

cdr
	self assertBound.
	^self term cdr!

ifBound: action
	self isBound ifTrue: [
		action value
	]!

ifNotBound: action
	self isBound ifFalse: [
		action value
	]!

isBound
	^term isNil not.
!

printOn: aStream
	aStream
		nextPut: $V;
		nextPut: $(;
		print: varName;
		nextPut: $,;
		space;
		print: term;
		nextPut: $).!

restoreVars
	| cp t |
	cp := V @ (self varName).
	t := cp term.
	self term: t.!

term
	^term!

term: t
	term := t!

term: t varName: n
	term := t.
	varName := n.
	dependentVars := Bag new.!

unify: other
	(other == self) ifTrue: [ ^true ].
	(other varInside: (self varName)) ifTrue: [ ^false ].
	^other unifyVariable: self!

unifyPair: pair
	self ifBound: [
		^(self term) unifyPair: pair
	].
	self term: pair.
	dependentVars do: [ :elem | elem unifyPair: pair. ].
	^true!

unifyValue: val
	self ifBound: [
		^(val value) = (self term value)
	].
	self term: val.
	dependentVars do: [ :elem | elem unifyValue: val. ].
	^true
	!

unifyVariable: other
	self ifNotBound: [
		(other varInside: (self varName)) ifTrue: [ ^false ].
	].
	other ifBound: [
		self ifBound: [
			^(self term) unify: (other term)
		].
		self term: (other term).
		^true
	].
	self ifBound: [
		^other unifyVariable: self
	].
	self addDependent: other.
	other addDependent: self.
	^true
	!

value
	self assertBound.
	^self term value!

varInside: v
	^self varName == v!

varName
	^varName! !
!V categoriesFor: #addDependent:!public! !
!V categoriesFor: #assertBound!public! !
!V categoriesFor: #car!public! !
!V categoriesFor: #cdr!public! !
!V categoriesFor: #ifBound:!public! !
!V categoriesFor: #ifNotBound:!public! !
!V categoriesFor: #isBound!public! !
!V categoriesFor: #printOn:!public! !
!V categoriesFor: #restoreVars!public! !
!V categoriesFor: #term!public! !
!V categoriesFor: #term:!public! !
!V categoriesFor: #term:varName:!public! !
!V categoriesFor: #unify:!public! !
!V categoriesFor: #unifyPair:!public! !
!V categoriesFor: #unifyValue:!public! !
!V categoriesFor: #unifyVariable:!public! !
!V categoriesFor: #value!public! !
!V categoriesFor: #varInside:!public! !
!V categoriesFor: #varName!public! !

!V class methodsFor!

@ varName
	Vars ifNil: [
		self cleanDict
	].
	Vars at: varName ifAbsentPutValue: (self new term: nil varName: varName).
	^Vars at: varName!

cleanDict
	Vars := Dictionary new!

restoreDict: dict
	Vars := dict!

saveDict
	^Vars deepCopy! !
!V class categoriesFor: #@!public! !
!V class categoriesFor: #cleanDict!public! !
!V class categoriesFor: #restoreDict:!public! !
!V class categoriesFor: #saveDict!public! !

"Binary Globals"!

L := Object fromBinaryStoreBytes: 
(ByteArray fromBase64String: 'IVNUQiAzIAYBAQBDAAAAAA==')!

