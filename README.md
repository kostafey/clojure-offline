# clojure-offline

The tools and notes, assigned to help in clojure development environment
configuration without internet connection.

## Requirements:

* [GNU Emacs](http://www.gnu.org/software/emacs/emacs.html) 24.
* [Leiningen](http://leiningen.org) 2.x
* [nrepl.el](https://github.com/kingtim/nrepl.el)
* [clomacs](https://github.com/kostafey/clomacs)

## Installation

* copy this repository to somewhere in your .emacs.d
* add require to your `.emacs`:

```lisp
(add-to-list 'load-path "~/.emacs.d/clojure-offline/src/elisp/")
(require 'clojure-offline)
```

## Usage

### Short story

How to add new dependency (dependencies).

Use `clojure-offline-create-script` elisp function, e.g.:

```lisp
(clojure-offline-create-script [ring/ring-core "1.2.0"])
```

or just select (mark) `[ring/ring-core "1.2.0"]` and run <br>
`M-x clojure-offline-create-script RET RET`

You can use the select (mark) the whole dependencies vector, like this
`[[ring/ring-core "1.2.0"]]`.

The resolving all dependences offline, in general, is hard, so this resource
tries to help with fetching the full list of the dependences:
[clojure-offline-web](https://clojureoffline-kostafey.rhcloud.com)
([source on github](https://github.com/kostafey/clomacs)).


### Long story

How to configure clojure development environment from scratch.

In short, you need to install leiningen and place all necessary artifacts to the
`%HOME%\.m2\repository\` directory.

#### 1. Install leiningen

You can download `lein` script as usal from
[leiningen.org](http://leiningen.org) and place it somewhere in the `PATH`
environment variable.

The actual location of the `leiningen-%LEIN_VERSION%-standalone.jar` is holds
in the `LEIN_JAR_URL` variable in the lein script (or can be seen in the
error log when you run lein self-install). Download and place this jar in the
`%HOME%\.lein\self-installs\`

To ensure lein is installed correctly you can run `lein` from the shell.

#### 2. Install lein-localrepo (leiningen plugin)

Download `lein-localrepo-<version>.jar` file and required `tools.cli`. The
probable location of the file can be obtained by evaluating the following elisp
script:

```lisp
(clojure-offline-create-script
    [[lein-localrepo "0.5.2"]
     [org.clojure/tools.cli "0.2.2"]]
     :install :manual)
```

Place `*.jar` and `*.pom` files of the `lein-localrepo` and `tools.cli` to the
apropriate folders `%HOME%\.m2\repository\<group-id>\<artifact-id>\<version>\`,
e.g. `%HOME%\.m2\repository\lein-localrepo\lein-localrepo\0.5.2\`

Install it as a global plugin in `~/.lein/profiles.clj`:

```clojure
{:user {:plugins [[lein-localrepo "0.5.2"]]}}
```

To ensure lein-localrepo is installed correctly you can run the shell:

`lein localrepo help`

So, the required environment is installed.

#### 3. Download and install all required libraries

Select (mark) `:dependencies` vector in the defproject expression of the
`project.clj` file, like here (selection is marked by # symbols):

```clojure
:dependencies #[[org.clojure/clojure "1.5.1"]
                [compojure "1.1.5"]
                [me.raynes/laser "1.1.1"]
                [mysql/mysql-connector-java "5.1.24"]
                [korma "0.3.0-RC5"]
                [lib-noir "0.4.9"]]#
```

Run `M-x clojure-offline-create-script RET RET`

The "wget part" of this script shold be executed on the internet-connected
computer. Then copy the received `*.jar` and `*.pom` files to the target
computer and run the rest part of the script from the directory with this files.

Repeat this for `:plugins` and `:dev` sections, if required.

#### WARNING!

There is no guarantee that the file is located in the printed path.
E.g. https://clojars.org/repo/org/clojure/clojure/1.5.1/clojure-1.5.1.jar is
a wrong path since `clojure-1.5.1.jar` is not hosted in the clojars.org
(yet). In this case, it is locatid in the maven central. If the both of this
paths are worng, you should find it manually. But if the jar is hosted in the
clojars.org, the printed url is likely correct.

#### CAVEEATS

There are some problems can take place when you run different `lein` tasks
(e.g. compile or run something). The problems and ideas to solve them are
described below.

* The error occurrence shows us what additional jars is required, e.g.:

```
Could not transfer artifact org.clojure:clojure:pom:1.5.1 from/to central
(http://repo1.maven.org/maven2/): repo1.maven.org
This could be due to a typo in :dependencies or network issues.
```

So, it should be installed:

```lisp
(clojure-offline-create-script ["org.clojure:clojure:pom:1.5.1"])
```

* Or several artifacts is needed:

```
error in process sentinel:
Could not start nREPL server:
Could not transfer artifact org.clojure:tools.nrepl:pom:0.2.1 from/to central
(http://repo1.maven.org/maven2/): repo1.maven.org
Could not transfer artifact clojure-complete:clojure-complete:pom:0.2.2
from/to central (http://repo1.maven.org/maven2/): repo1.maven.org
This could be due to a typo in :dependencies or network issues.
```

Again:

```lisp
(clojure-offline-create-script ["org.clojure:tools.nrepl:pom:0.2.1"
                                "clojure-complete:clojure-complete:pom:0.2.2"])
```

## License

Copyright © 2013 Kostafey <kostafey@gmail.com>

Distributed under the General Public License, version 2.
