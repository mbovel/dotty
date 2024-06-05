# StackOverflow Exception on Highlight
When there is too much instruction the Sytax Hyghlight can't make the tree and launch a StackOverflow Error

To fix the problem let's catch the Overflow and return the entry string with an error message

That line triggers the error : TreeHighlighter.highlight(trees)

Since there is some code after there is 2 approches :

1. Put the try on everything starting from that line and the catch in the end
```
case(e): StackOverflowError =>
          println("Some stack overflow")
          in
```

2. Put the try only on the line that can throw the error and a return inside the catch
```
  try
    TreeHighlighter.highlight(trees)
  catch
    case(e): StackOverflowError =>
      println("Some stack overflow")
      return in
```

The first solution has the merit of staying functional but we lose a bit of precision on what can cause the error, if a StackOverflows occurs at a different unexpected case then we'll treat it the same way.

The second solution lose the functional aspect with the return but is very close to the problem, the try block has only 1 line inside so there is less doubt on what triggers the exception.
