# A bytecode compiler

From a very high level, the way the compiler works is very simple. It consists of three main parts:

- the parser
- the compiler
- the VM

## The parser

The goal of a compiler, in essence, is to produce a machine code from an output of plain text. The natural first step of this process is to
figure out how specifically to process the plain text, so that it may be run through our program and eventually interpreted, and executed.

A logical an convenient way to process plain text is a method called _tokenization_.

Tokenization is the process of convering plain text into a pre-defined subset of valid user inputs which may then be processed by the compiler.
This is useful because while plain text itself can have an infinite number of combinations, a well defined set of tokens acts as a clear way to
distinguish between a manageable list of valid inputs, and invalid inputs, which would simply be anything not _tokenizable_.

If the user inputs something in plain text that can't be turned into a token, we know immediately that the code is invalid.
Otherwise, there can exists within the compiler a (somewhat) straightforward control flow which is

if this token -> do this
if this other token -> do this instead

Ensuring that the tokens themselves are in a sequence that makes any kind of sense is another level of abstraction, which the parser needn't worry about.

## The compiler

We have established that the compiler has a well defined set of inputs. These are the tokens that are produced from the parser.

What then are the outputs of the compiler?
The hint is in the name of the type of interpreter that Boop is an example of: it produces _bytecode_!

Bytecode, in this context at least, is simply a series of u8 _bytes_ that we can enumerate and ship off to the VM for
interpretation. Basically, what tokens are to the compiler, bytecode is to the interpreter/VM: a well defined set of
valid values that can be used as inputs.

Bytecode can also be described as the "instructions" of our interpreter. Things like "ADD" and "RETURN" are what our VM
will need know out program is asking it to do before it can actually do it.

Therefore, the compiler is really just a long algorithm for taking in an ordered list of Tokens and producing an ordered list of
bytecode instructions.

## the VM

This stands for "virtual machine". It's called that because, instead of relying on our physical machine, or cpu to process
instructions that it understands, such as x86 or ARM assembly code, we are simulating out own machine through software which can understand _our_ custum bytecode.

by creating a virtual machine, we can take a language like rust, which is already set up to run on multiple types of platforms, use its functionality communicate to the physical machines of our end users. We only have to write and ship a single program which tells Rust what is it going to be doing for us.

From a high level then. Heres what's happening when our interpreter is in action. Let's take a simple program as an example

1 + 1

1. parser turns the plain text into something like [TOKEN_NUMBER, TOKEN_ADD, TOKEN_NUMBER]
2. compiler takes these tokens an produces something like [OP_NUMBER, OP_ADD, OP_NUMBER]
3. VM runs the rust code

1 + 1

Which isn't very exciting considering the code really looks the same...
