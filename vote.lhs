<h1>Virtual Round Robin Voting</h1>

This <a href="http://www.haskell.org" >Haskell</a> code implements <a href="http://bolson.org/voting/VRRexplaination.pdf" >virtual round robin voting</a>, minus the final decision function.

<h2>Imports</h2>

As a first step, we import the LinearAlgebra tools from the hmatrix library, in addition to the following helper functions:
<ol>
<li>The 'fromBool' function turns False into 0 and True into 1, which we will need later.</li>
<li>The 'elemIndex' function returns the 0-based index of an element in a list (assuming it can be found in the list).</li>
</ol>

> import Numeric.LinearAlgebra
> import Foreign.Marshal.Utils (fromBool)
> import Data.List (elemIndex)

<h2>Definitions</h2>

Next we define the list of contenders that our voters can choose from.  The index of each contender matters; so, for example, "Let's watch a movie" is the zero-th element, which we will use in a moment.

> contenders :: [String]
> contenders = ["Let's watch a movie", "Let's build a fort", "Let's go swimming"]

Here is where we pretend people voted.  'v1' is voter number 1, 'v2' is voter number 2, etc.  Voter #1 prefers building a fort (contender item 1) to all other options, and prefers swimming (contender item 2) to watching a movie (contender item 0).

> v1 :: [Int]
> v1 = [1, 2, 0] -- Active person

Voter #2 prefers watching a movie (0) to building a fort (1), and building a fort (1) to swimming (2).

> v2 :: [Int]
> v2 = [0, 1, 2] -- Movie watcher

Voter #3 prefer building a fort (1) to watching a movie (0), and watching a movie (0) to swimming (2).

> v3 :: [Int]
> v3 = [1, 0, 2] -- Fort builder

Putting all the voting together, we get a list of ballots.  Since each ballot above is a list of integers, a list of ballots is a list of lists of integers, denoted '[[Int]]' in the type definition.

> ballots :: [[Int]]
> ballots = [v1, v2, v3]

Here's the main part of our code: a function that takes an ordered list of integers and turns it into a preference matrix.  The idea here is that we want a list like '[1, 0, 2]' (as in the example of the Fort Builder vote, above) to turn into a matrix, like this:

[0 0 1]<br>
[1 0 1]<br>
[0 0 0]

We accomplish this using the 'buildMatrix' function which takes the number of rows and columns (3 in our case), and a 'builder' function.  The builder function takes a (row, col) pair and for each row and column combination, produces the number value that should go in that spot.  So in our example above, when the builder function gets row 0, column 0, it should return 0 (the upper-left corner of the matrix).  But when it gets row 1, column 0, it should return 1.  And so on down the list of rows and columns.

> ordered :: [Int] -> Matrix Double
> ordered prefs = buildMatrix n n builder
>   where
>     n = length prefs
>     builder (row, col) | row == col = 0 -- zeros down the diagonal
>     builder (row, col) =
>       case elemIndex row prefs of
>         Just a  -> case elemIndex col prefs of
>                 Just b  -> fromBool(a < b)
>                 Nothing -> error "Couldn't find it"
>         Nothing -> error "Couldn't find it"

The tally function just adds up all of the matrices that we want to be part of the vote.  We do this using the foldr function (a very common Haskell function), as well as the map function (another very common function).

The hmatrix library gives us a '><' operator (that kind of looks like an X) to create an n by m matrix.  We use this operator to construct a 3 by 3 matrix of zeros called 'zeroMatrix'.

You'll also see the 'buildMatrices' function which takes the list of ballots and turns them into a list of matrices using the 'ordered' function we defined previously.

> tally = foldr (+) zeroMatrix ballotMatrices
>   where zeroMatrix = (3><3 $ repeat 0)
>         ballotMatrices = map ordered ballots

<h2>Main Function</h2>

Last but not least, we show our results of the tally as a 3x3 matrix.  Haskell has a special function called 'main' that is the starting point recognized by the compiler.  'putStrLn' prints a string as the output of our program.  The output of interest, in our case, is the 'tally' matrix shown as a string.

> main = do
>   putStrLn $ show tally