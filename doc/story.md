One day,Dog Saburo has the following problem.

How many sequences of consective natual numbers that contain more than two integers and its sum is equal to N.
For example,if N=9,there are [2,3,4],[4,5],so answer is 2.
http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=2197

Saburo also has two examples given as sample.
If N = 9 then answer is 2.
If N = 500 then answer is 3.


Saburo opens editor and writes program using Shiranui Language.
At first,Saburo decides function's name.

~~~~~~
def solve(int N) -> int:
    return void
~~~~~~

Now the editor shows that there is type error.
Because,"solve" is defined as int->int,but program itself return void.

Saburo knows a few example,so writes it down.
#- means "this line is testcase".

~~~~~~
#- (9) -> 2    (error)
#- (500) -> 3  (error)
def solve(int N) -> int:
    return void
~~~~~~

Saburo didn't edit "source code",so the type error is not resolved.
Lines of testcases are colored red.
So,Saburo defined variable "ret" and return it.

~~~~~~
#- (9) -> 2    (0: incorrect)
#- (500) -> 3  (0: incorrect)
def solve(int N) -> int:
    int ret = 0
    return ret
~~~~~~

There is no type error.
But of cource,this it not "right" algorithm.
Lines of test cases are colored red.

And Saburo knows,he needs "sum_of_consectives" to write "solve".
He learned it in his junior high school,but he seemed to misremember it.

~~~~~~
# return sum of [0,N] <- this is just comment.
def sum_of_consectives(int N) -> int:
    return N*(N-1)/2

#- (9) -> 2    (0: incorrect)
#- (500) -> 3  (0: incorrect)
def solve(int N) -> int:
    int ret = 0
    return ret
~~~~~~

His "sum_of_consectives" function is meaningless if N < 0.
So,he write precondition of it.

~~~~~~
# return sum of [0,N]
def sum_of_consectives(int N) -> int:
    precondition:
        assert(0 <= N)
    return N*(N-1)/2
~~~~~~

Currently,he doesn't have testcases.
So,he wants to make it.

~~~~~~
#- (3)
# return sum of [0,N]
def sum_of_consectives(int N) -> int:
    precondition:
        assert(0 <= N)
    return N*(N-1)/2
~~~~~~

During writing testcase,if function is executable (i.e there is no syntax or type error),
its result will appear rightside of testcase.

~~~~~~
#- (3) -> 3? (?)
# return sum of [0,N]
def sum_of_consectives(int N) -> int:
    precondition:
        assert(0 <= N)
    return N*(N-1)/2
~~~~~~

When modify leftside of testcase,rightside will be refreshed too.

~~~~~~
#- (5) -> 10? (?)
# return sum of [0,N]
def sum_of_consectives(int N) -> int:
    precondition:
        assert(0 <= N)
    return N*(N-1)/2
~~~~~~

Saburo knows,the result is incorrect.
So,move cursor to leftside and input "correct" answer.
(or,use keybind like C-i)

~~~~~~
#- (5) -> 15 (10: incorrect)
# return sum of [0,N]
def sum_of_consectives(int N) -> int:
    precondition:
        assert(0 <= N)
    return N*(N-1)/2
~~~~~~

Saburo is wise enough to add some more cases.
(There is convinient keybiding like C-return to add multiple cases.
It will copy current line and paste it to following line.)

~~~~~~
#- (1) -> 0? (?)
#- (5) -> 15 (10: incorrect)
# return sum of [0,N]
def sum_of_consectives(int N) -> int:
    precondition:
        assert(0 <= N)
    return N*(N-1)/2
~~~~~~

And C-return.

~~~~~~
#- (1) -> 0? (?)
#- (1) -> 0? (?)
#- (5) -> 15 (10: incorrect)
# return sum of [0,N]
def sum_of_consectives(int N) -> int:
    precondition:
        assert(0 <= N)
    return N*(N-1)/2
~~~~~~

Modify and C-return and modify and...

~~~~~~
#- (1) -> 0? (?)
#- (2) -> 1? (?)
#- (3) -> 3? (?)
#- (4) -> 6? (?)
#- (5) -> 15 (10: incorrect)
# return sum of [0,N]
def sum_of_consectives(int N) -> int:
    precondition:
        assert(0 <= N)
    return N*(N-1)/2
~~~~~~

Saburo thought,"Oh,in many cases,my program returned incorrect answer.)
In this simple situation,Saburo does't want to write correct answer each case.
So,Saburo starts fixing this program.

Saburo also notices,returned value is always less than correct answer.
So,he'll modify term (N-1).
1. Remove "-"...there is syntax error.

~~~~~~
#- (1) -> error? (?)
#- (2) -> error? (?)
#- (3) -> error? (?)
#- (4) -> error? (?)
#- (5) -> 15 (error)
# return sum of [0,N]
def sum_of_consectives(int N) -> int:
    precondition:
        assert(0 <= N)
    return N*(N1)/2
~~~~~~

2. And insert "+".

~~~~~~
#- (1) -> 1? (?)
#- (2) -> 3? (?)
#- (3) -> 6? (?)
#- (4) -> 11? (?)
#- (5) -> 15 (15: correct)
# return sum of [0,N]
def sum_of_consectives(int N) -> int:
    precondition:
        assert(0 <= N)
    return N*(N+1)/2
~~~~~~

3. Check.
Yes!Line "#- (5) -> 15 (15: correct)" is colored green!
Other cases seem to be good.
Saburo finally approves returned value of some cases as correct value.
(Keybinding like C-o)

~~~~~~
#- (1) -> 1 (1: correct)
#- (2) -> 3 (3: correct)
#- (3) -> 6 (6: correct)
#- (4) -> 11 (11: correct)
#- (5) -> 15 (15: correct)
# return sum of [0..N]
def sum_of_consectives(int N) -> int:
    precondition:
        assert(0 <= N)
    return N*(N+1)/2
~~~~~~

Saved testcases will be used when he modified sum_of_consectives.
If he need to write more efficient function or find bug in sum_of_consectives,they are helpful.
(Yes,fixing bug will make new bug occasionally.)
Or,it will be document for user.

OK.Now he has "correct" sum_of_consectives.
Next,he want to write "sum_up" the helper function.

At first,he write following function.

~~~~~~
# return sum of [S..T]
def sum_up(int S,int T) -> int:
    precondition:
        assert(S <= T)
    return sum_of_consectives(T) - sum_of_consectives(S)
~~~~~~

And adds testcase...

~~~~~~
#- (1,1) -> 0? (?)
#- (1,2) -> 2? (?)
# return sum of [S..T]
def sum_up(int S,int T) -> int:
    precondition:
        assert(S <= T)
    return sum_of_consectives(T) - sum_of_consectives(S)
~~~~~~

Hmm.It seems to be incorrect.
He thinks, "Oh! [0,T] - [0,S] = [S+1,T]!".
So,term "sum_of_consectives(S)" must be "sum_of_consectives(S-1)"...

~~~~~~
#- (1,1) -> 1? (?)
#- (1,2) -> 3? (?)
# return sum of [S..T]
def sum_up(int S,int T) -> int:
    precondition:
        assert(S <= T)
    return sum_of_consectives(T) - sum_of_consectives(S-1)
~~~~~~

OK,let's add some tests.

~~~~~~
#- (1,1) -> 1? (?)
#- (1,2) -> 3? (?)
#- (0,2) -> error? (?)
#- (2,5) -> 14? (?)
# return sum of [S..T]
def sum_up(int S,int T) -> int:
    precondition:
        assert(S <= T)
    return sum_of_consectives(T) - sum_of_consectives(S-1)
~~~~~~

Hmm? If input is (0,2), error occured but,the others is correct..

OK,let's "inspect" it.
"Inspect" means,jump to its context.

Move to "#- (0,2) -> error? (?)" line and press C-s.
It is keybinding for "inspect this testcase".

~~~~~~
#- (1,1) -> 1? (?)
#- (1,2) -> 3? (?)
#- (0,2) -> error? (?)
#- (2,5) -> 14? (?)
# return sum of [S..T]
def sum_up(int S,int T) -> int:
    precondition:
        assert(S <= T) # colored green.
    return sum_of_consectives(T) - sum_of_consectives(S-1)
~~~~~~

Assert line will be colored green.
Because 0 <= 2.
But "sum_of_consectives(S-1) on right side" will be colored red.
It violates precondition of sum_of_consectives.
After moving cursor to sum_of_consectives(S-1),then editor said,
"Make sure '0 <= S-1'!!!" ,or show balloon or something like that.
Saburo knows,S=0,so S-1 will be -1.

<!-- Hmm,error is in sum_of_consectives.
 Inspect it.C-s on @ at sum_of_consectives.(hide testcases of sum_of_consectives.)
 Edtior will make you jump to sum_of_consectives definition. -->

He noticeds,if S=0,sum_up(S,T) == sum_of_consectives(T).
He added it to code.

~~~~~~
#- (1,1) -> 1? (?)
#- (1,2) -> 3? (?)
#- (0,2) -> 3 (?)
#- (2,5) -> 14? (?)
# return sum of [S..T]
def sum_up(int S,int T) -> int:
    precondition:
        assert(S <= T)
    if S == 0:
        return sum_of_consectives(T)
    else:
        return sum_of_consectives(T) - sum_of_consectives(S-1)
~~~~~~

Saburo is careful enough to add more tests.
"If S < 0..."

~~~~~~
#- (1,1) -> 1? (?)
#- (1,2) -> 3? (?)
#- (0,2) -> 3 (?)
#- (2,5) -> 14? (?)
#- (-1,5) -> error? (?)
# return sum of [S..T]
def sum_up(int S,int T) -> int:
    precondition:
        assert(S <= T)
    if S == 0:
        return sum_of_consectives(T)
    else:
        return sum_of_consectives(T) - sum_of_consectives(S-1)
~~~~~~

Saburo learned that sum_of_consectives(S-1) violates its contract if S is less than 0
                                                                       from (0,2) case.
So,Saburo add precondition.

~~~~~~
#- (1,1) -> 1? (?)
#- (1,2) -> 3? (?)
#- (0,2) -> 3? (?)
#- (2,5) -> 14? (?)
#- (-1,5) -> input_error? (?)
# return sum of [S..T] (S <= T and 0 <= S.)
def sum_up(int S,int T) -> int:
    precondition:
        assert(S <= T)
        assert(0 <= S)
    if S == 0:
        return sum_of_consectives(T)
    else:
        return sum_of_consectives(T) - sum_of_consectives(S-1)
~~~~~~

error? will be changed to input_error?.
There is a big difference between error? and input_error?.
error? means,input can be approved,but error occured somewhere.
input_error? means,it is meaningless input.i.e there are failed assert in precondition of its function.

OK,Saburo approved all testcase of sum_up.

~~~~~~
#- (1,1) -> 1 (1: correct)
#- (1,2) -> 3 (3: correct)
#- (0,2) -> 3 (3: correct)
#- (2,5) -> 14 (14: correct)
#- (-1,5) -> input_error (input_error: correct)
# return sum of [S..T] (S <= T and 0 <= S.)
def sum_up(int S,int T) -> int:
    precondition:
        assert(S <= T)
        assert(0 <= S)
    if S == 0:
        return sum_of_consectives(T)
    else:
        return sum_of_consectives(T) - sum_of_consectives(S-1)
~~~~~~

OK.Now Saburo has correct "sum_up".
Finally,he starts writing solve.

His algorithm is like this.

~~~~~~
cnt = 0
for all consectives [s,t]:
    compute sum_up(s,t).
    if sum is equal to N:
        cnt = cnt + 1
~~~~~~

So,he wrote this.

~~~~~~
#- (9) -> 2    (3: incorrect)
#- (500) -> 3  (4: incorrect)
def solve(int N) -> int:
    int ret = 0
    for s in [1..N]:
        for t in [s..N]:
            if sum_up(s,t) == N:
                ret++
    return ret
~~~~~~

Hmm,there is mistake.
So,he inspects it.

Move "#- (9) -> 2 (3: incorrect)" and C-s.

~~~~~~
#- (9) -> 2 (3: incorrect)
#- (500) -> 3 (4: incorrect)
def solve(int N) -> int:
    int ret = 0
    for s in [1..N]:
        for t in [s..N]:
            if sum_up(s,t) == N:
                ret++
    return ret
~~~~~~

At first,Saburo want to know when "ret++" called.
So,insert "printf" statement.
In generally,it is called "printf-debugging".

~~~~~~
#- (9) -> 2 (3: incorrect)
#- (500) -> 3 (4: incorrect)
def solve(int N) -> int:
    int ret = 0
    for s in [1..N]:
        for t in [s..N]:
            if sum_up(s,t) == N:
                print(s,t)
                ret++
    return ret
~~~~~~


