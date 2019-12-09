# Delete

The delete command can delete both terms and types, as long as it's given an
unambiguous name.

First, let's make sure it complains when we try to delete a name that doesn't
exist.

```ucm:error
.> delete foo
```

Now for some easy cases. Deleting an unambiguous term, then deleting an
unambiguous type.

```unison
foo = 1
unique type Foo = Foo Nat
```

```ucm
.> add
.> delete foo
.> delete Foo
.> delete Foo.Foo
```

How about an ambiguous term?

```unison
foo = 1
```

```ucm
.a> add
```

```unison
foo = 2
```

```ucm
.b> add
.a> merge .b
```

```ucm:error
.a> delete foo
```

I can force my delete through by re-issuing the command.

```ucm
.a> delete foo
```

Let's repeat all that on a type, for completeness.

```unison
unique type Foo = Foo Nat
```

```ucm
.a> add
```

```unison
unique type Foo = Foo Nat
```

```ucm
.b> add
.a> merge .b
```

```ucm:error
.a> delete Foo
```

```ucm
.a> delete Foo
```

```ucm:error
.a> delete Foo.Foo
```

```ucm
.a> delete Foo.Foo
```

Finally, let's try to delete a term and a type with the same name.

```unison
foo = 1
unique type foo = Foo Nat
```

```ucm
.> add
```

```ucm:error
.> delete foo
```

```ucm
.> delete foo
```
