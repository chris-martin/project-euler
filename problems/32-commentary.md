Let `|n|` denote the number of digits in `n`.

We're looking for `(x, y, z)` such that `x × y = z`.

From `|x| + |y| + |z| = 9` and `y < z`, we obtain the following:

*Lemma 1*: `|z| >= 4`

*Lemma 2*: `|x| + |y| <= 5`

*Lemma 3*: `|y| <= 4`

Assume `x < y`, since interchanging `x` and `y` does not affect `|z|`.

---

Consider separately the cases of `|x|`.

Where `|x| = 1`
---------------

`|y| >= 4` because `9 * 987 < 12345`

`|y| <= 4` by Lemma 3

Therefore `|y| = 4`

Where `|x| = 2`
---------------

`|y| >= 3` because `98 * 98 < 12345`

`|y| >= 3` using Lemma 1

Therefore `|y| = 3`

Where `|x| >= 3`
---------------

`|y| >= 3` from the assumption `|x < y|`

This contradicts Lemma 2, so this case is vacuous.

---

So we're left with only two ways to break up the nine digits:

* `x-yyyy-zzzzz`
* `xx-yyy-zzzzz`

And in either case, `|z| = 5`.
