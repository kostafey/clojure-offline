# clojure-offline

The tools and notes, assigned to help configure clojure development environment
without internet connection.

## Usage

How to work with clojure offline

### Short story

Use `clojure-offline-create-script` elisp function e.g.

```lisp
(clojure-offline-create-script [[ring/ring-core "1.2.0"]])
```

or just select (mark) `[[ring/ring-core "1.2.0"]]` and run <br>
`M-x clojure-offline-create-script RET RET`

The resolving all dependences offline, in general, is hard, so this resource
tries to help with it:

https://clojureoffline-kostafey.rhcloud.com

### Long story

#### 1. Install leiningen

You can download `lein` script as usal from `leiningen.org` and place it
somewhere in the `PATH` environment variable.

The actual location of the `leiningen-%LEIN_VERSION%-standalone.jar` is holds
in the `LEIN_JAR_URL` variable in the lein script (or can be seen in the
error log when you run lein self-install). Download and place this jar in the
`%HOME%\.lein\self-installs\`

To ensure lein is installed correctly you can run `lein` from the shell.

#### 2. Install lein-localrepo (leiningen plugin)

Download `lein-localrepo-<version>.jar` file. The probable location of the
file can be obtained by evaluating the following elisp script:

```lisp
(clojure-offline-create-script
    [[lein-localrepo "0.5.2"]
     [org.clojure/tools.cli "0.2.2"]])
```

(clojure-offline-guess-clojars-url [lein-localrepo "0.5.2"])
(clojure-offline-guess-clojars-url [org.clojure/tools.cli "0.2.2"])

Place jar and pom files to the
`%HOME%\.m2\repository\<group-id>\<artifact-id>\<version>\` folder, e.g.
`%HOME%\.m2\repository\lein-localrepo\lein-localrepo\0.5.2\`

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
computer. Then copy the received *.jar and *.pom files to the target computer
and run the rest part of the script from the directory with this files.

Repeat this for `:plugins` and `:dev` sections, if required.

#### WARNING!

There is no guarantee that the file is located in the printed path.
E.g. https://clojars.org/repo/org/clojure/clojure/1.5.1/clojure-1.5.1.jar is
a wrong path since clojure-1.5.1.jar is not hosted in the clojars.org
(yet). In this case, it is locatid in the maven central. If the both of this
paths are worng, you should find it manually. But if the jar is hosted in the
clojars.org, the printed url is likely correct.

#### CAVEEATS

The problems and ideas to solve them during installations of the
[lein-ring "0.8.3"] plugin (as an example) are described below:

In the simple cases when you run, e.g.:

* `M-x nrepl-jack-in`
* `lein help`
* `lein uberjar`
* `lein ring uberwar`
* `lein ring server`

the error occurrence shows us what additional jars is required, e.g.:

```
Could not transfer artifact org.clojure:clojure:pom:1.5.1 from/to central
(http://repo1.maven.org/maven2/): repo1.maven.org
This could be due to a typo in :dependencies or network issues.
```

So, it should be installed:

```clojure
(clojure-offline-create-script ["org.clojure:clojure:pom:1.5.1"])
```

Or several artifacts is needed:

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

```clojure
(clojure-offline-create-script ["org.clojure:tools.nrepl:pom:0.2.1"
                                "clojure-complete:clojure-complete:pom:0.2.2"])
```

The hard problem looks like this:

```
Problem loading: Could not locate leinjacker/deps__init.class or
leinjacker/deps.clj on classpath:
```

So, install `leinjacker` with all dependences and try again. The problem
remains...  It is not mean that `leinjacker' is not installed. It is not mean
that leinjacker's dependences is not installed. It is not mean that
dependences of the leinjacker's dependences is not installed... and so on...
ok :)

In my case the problem was in the `lein-ring' (with empty *.pom file in the
.m2 directory) dependences, wich is requires `leinjacker' (wich is already
installed) and `org.clojure/data.xml` (wich is not installed yet in this
example).

To see the full list of the dependences you can use
https://clojureoffline-kostafey.rhcloud.com.

BTW, if the `lein-ring's *.pom file were available and correct (not empty) in
the maven cashed directory (.m2) you will see the following error insted of
the previous:

```
Could not transfer artifact org.clojure:data.xml:pom:0.0.6 from/to central
(http://repo1.maven.org/maven2/): repo1.maven.org
This could be due to a typo in :dependencies or network issues.
```

In the case where all jar dependences is resolved the following problem
occurs:

```
Could not locate clojure/core/contracts/impl/transformers__init.class or
clojure/core/contracts/impl/transformers.clj on classpath:
```

The only way to resolve - provede all necessary pom file. Re-run and, the
actual problem shoud be shown:

```
Could not transfer artifact org.clojure:pom.contrib:pom:0.0.26 from/to
central (http://repo1.maven.org/maven2/): repo1.maven.org
```

Copy from
https://maven-us.nuxeo.org/nexus/content/groups/public/org/clojure/pom.contrib/0.0.26/pom.contrib-0.0.26.pom
to
`~/.m2/repository/org/clojure/pom.contrib/0.0.26/pom.contrib-0.0.26.pom`

```
Could not transfer artifact org.sonatype.oss:oss-parent:pom:5 from/to
central (http://repo1.maven.org/maven2/): repo1.maven.org
```

Copy from
http://repo1.maven.org/maven2/org/sonatype/oss/oss-parent/5/oss-parent-5.pom
to
`~/.m2/repository/org/sonatype/oss/oss-parent/5/oss-parent-5.pom`


## License

Copyright © 2013 Kostafey <kostafey@gmail.com>

Distributed under the General Public License, version 2.
