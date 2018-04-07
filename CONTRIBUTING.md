# Contributing to `hs-init`

## :wave: Greetings Traveler!

I'm really glad you're reading this, I really appreciate the effort you're taking. Thank you for your help in making this tool awesome!:sparkles:

### How to contribute
#### Report bugs or feature request
If you found any bugs or have any proposals how to make this project better
don't hesitate to create issues [here][issues] in free format.

#### Create a PR
We love pull requests from everyone :heart:.
To get started with this you should first fork, then clone the repo:

    git clone git@github.com:your-username/hs-init.git

Make your changes and consider the following check list to go through before submitting your pull request.

#### :white_check_mark: Check list
- [ ] `hs-init` compiles
- [ ] New/fixed features work as expected
- [ ] Old features do not break after the change
- [ ] There are no warnings during compilation
    **_Note:_** You can use `stack ghc -- -Wall hs-init.hs ` command
- [ ] `hlint hs-init.hs` output is: _No Hints_
- [ ] Code is stylish :lipstick:
    **_Note:_** `stylish-Haskell -I hs-init.hs`
- [ ] Commit messages are in the proper format. If the commit addresses an issue start
      the first line of the commit with the issue number in square parentheses.
    **_Example:_** `[#42] Short commit description`

After all above is done commit and push to your fork.
Now you are ready to [submit a pull request][pr].


----------
Thanks for spending time on reading this contributing guide! :sparkling_heart:

[pr]: https://github.com/kowainik/hs-init/compare/
[issues]: https://github.com/kowainik/hs-init/issues/new
