These Makefile fragments will be sufficient to generate an interactive GitHub
Pages site with an interactive JavaScript frontend for your library.  To use it,
first edit your Makefile to have an entry to activate the rules:

    include mk/Makefile.github

Then get your local OPAM setup in shape by pinning the relevant version of
iocamljs:

    make iocamljs-depends

You now need to install your development library and build an IOCaml notebook
by running a local site and writing the content via your web browser:

    make iocamljs-dev

Call your notebook `index` so that it'll be picked up by default when it is
eventually deployed to GitHub Pages.

Once you are happy with it, run the following to clone a `gh-pages` branch
and push to it.  You should run this every time there's an API change or a 
release of your library.

    make iocamljs-site

Before you do this, edit the `sh` fragments in `mk/` with your library name
and deployment URL base on GitHub.  This will eventually be automated...

Credits:
- Andrew Ray for iocaml
- Peter Zotov for the `gh-pages` target
- Anil Madhavapeddy for these lashed together shell scripts.
