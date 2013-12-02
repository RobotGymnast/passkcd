passkcd is based on [xkcd comic 936](http://xkcd.com/936/), which posits that a series of
random words can be as secure as conventional passwords (or more), and is far easier to
remember.

Currently it uses `mkRandomIO` from [system-random-effect](
http://hackage.haskell.org/package/system-random-effect).
Any vulnerabilities or inflexibilities are likely propogated to passkcd.
(For instance, an attacker with your dictionary file who knows when you
generated the password will be able to recreate it).
