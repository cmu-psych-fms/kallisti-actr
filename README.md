# Lisp JSON over HTTP server for a project whose name must not be uttered

> The language is that of Mordor, which I will not utter here.
> – J R R Tolkein, *The Lord of the Rings*

This hunk of code is dedicated to the memory of the Power Macintosh 7100, b. March 24, 1994, d. January 6, 1996.

This is a simple HTTP server implemented in Common Lisp that takes JSON requests using the POST method
to send a single JSON value, calls a Lisp function on the Lisp version of that value, converts the
return value of that function to JSON and returns it to the caller. It was created for use in the project who name must not be uttered.
but likely will have use in other projects, too. Various choices made here are simply best guesses for
what we want, and will undoubtedly evolve as we negotiate the needs of our caller(s). When using this
for other projects it will probably be best to fork it and modify it to taste for those other projects.
It currently uses port 9899, but this can, of course, be easily changed.

This has only been tested in [SBCL](https://www.sbcl.org) both on MacOS on M4 and on Ubuntu Linux on x86-64 but will probably work in other modern Common Lisp
implementations with networking support, such as [LispWorks](https://www.lispworks.com), [Allegro CL](https://franz.com/products/allegro-common-lisp/),
[CCL](https://ccl.clozure.com) and [ABCL](https://www.abcl.org/doc/abcl-user.html), on any supported hardware and OS platform, possibly with some minor tweaks required.


## Installation

* Ensure SCBL and [Quicklisp](https://www.quicklisp.org/beta/) are installed.

* Cone this repo, and `cd` into it.

* Start it by running `./run.sh`

* Test that it is working by running, in a different shell, `curl -d @sample-input.json http://localhost/decision`
which makes an HTTP request to the server sending the contents of the file `sample-input.json` as POST data.

* This should print something like

    {"actions":
        [{"action_id":"d44cc237-9e09-4cb9-97aa-6f32831df844","probability":1.0},
        {"action_id":"bf93c4cc-6063-458e-afa4-b024f5c9abb6","probability":0.0}]}


## Docker container

If you don't want to install SBCL and so on on a local machine it is also possible to build and run a Docker container, defined by the Dockerfile in the repo. For example,

    (dfm) dfm@carlisle:~/work/bha/json-http$ docker build -t json-http .
    [+] Building 42.3s (21/21) FINISHED                                                                      docker:default
    ... [lots of Docker spew elided here] ...
     => exporting to image                                                                                             0.3s
     => => exporting layers                                                                                            0.3s
     => => writing image sha256:1acced5298f4b3825d2b28bdde0be6ed3b2a518ec88fca3822cfb1860f8497f9                       0.0s
     => => naming to docker.io/library/json-http

Then run

    (dfm) dfm@carlisle:~/work/bha/json-http$ sudo docker run -p 9899:9899 json-http
    This is SBCL 2.5.3, an implementation of ANSI Common Lisp.
    More information about SBCL is available at <http://www.sbcl.org/>.

    SBCL is free software, provided as is, with absolutely no warranty.
    It is mostly in the public domain; some portions are provided under
    BSD-style licenses.  See the CREDITS and COPYING files in the
    distribution for more information.
    ... [Quicklisp spew elided here] ...
    <INFO> [17:06:52] json-http - Started #<EASY-ACCEPTOR (host *, port 9899)>

And in a different shell,

    (dfm) dfm@carlisle:~/work/bha/json-http$ curl -d @sample-input.json http://localhost:9899/decision
    {"actions":[{"action_id":"d44cc237-9e09-4cb9-97aa-6f32831df844","probability":1.0},{"action_id":"bf93c4cc-6063-458e-afa4-b024f5c9abb6","probability":0.0}]}


## NOTE THAT EVERYTHING BELOW THIS POINT IT LIKELY OUT OF DATE

Like the woodsman too busy chopping down trees to stop and sharpen their axe, we can fix this right now 'causes we're running too fast against a constant series of over-aggressive deadlines and thus
falling further and further behind.

## Hooking up a Lisp function to process the JSON

***Important note: this section is currently woefully out of date, and needs to be updated once we get past the currently pending demo which
is forcing us to do things quickly and sloppily instead right****

When the server receives a request it calls the function named `run-model` in the `cl-user` package, passing it
a Lisp from of the JSON request as its sole argument. This function is in the `cl-user` package because (a) most
ACT-R programmers work in the that package, and (b) even when not using ACT-R that is the preferred package of
the folks who I expect will be using this. Note that the server code itself is isolated in a different package `json-http`
(with the abbreviated nickname `jh'), from which are exported `run-standalone`, `jh:start-server` and `jh:stop-server`.
When developing a `cl-user::run-model` function it will typically be most convenient to `load` the single source file
`http-server.lisp` and call `jh:start-server` from Lisp.

The Lisp form of JSON is constructed recursively as follows:

* JSON objects are represented by Lisp plists, the keys of which are Lisp keywords.

* The keys are constructed from the JSON key strings by replacing underscores by hyphens and interning their
all upper case version in the `keyword` package

* JSON strings, other than keys in objects, are passed through unchanged

* JSON integers are passed through unchanged

* JSON floating point numbers are passed through as Lisp floats, whose precision is determined by the current value of `*read-default-float-format*`

* JSON lists are converted to Lisp simple vectors; Lisp lists are *not* used because of the ambiguity that could result with plists representing JSON objects

* The JSON Booleans `true` and `false` are converted to the Lisp symbols `t` and `nil`, repsectively

* The JSON `null` value is convert to the Lisp symbol `null`, which is exported from the `common-lisp` package, and so is typically available in all pacakges;
this avoids the ambiguity that would result from represent both JSON `false` and JSON `null` by the same Lisp value `nil`

Thus, the example input from Ben's first pass at description of requests that will be made in the JAG ⇔ Cognitive Model communication
(which is available in `sample-input.json` in the repo),

    {
        "actions": [
            {
                "actors": ["A"],
                "urn": "urn:centipede:take",
                "name": "Take",
                "id": "d44cc237-9e09-4cb9-97aa-6f32831df844",
                "inputs": [
                    { "name": "turn", "type": "int", "value": 0 },
                    { "name": "pot", "type": "int", "value": 5 }
                ],
                "outputs": [
                    { "name": "game_over", "type": "boolean" },
                    { "name": "payoff", "type": "int" }
                ],
                "expected_cost": null,
                "expected_value": { "payoff": 4 }
            },
            {
                "actors": ["A"],
                "urn": "urn:centipede:push",
                "name": "Push",
                "id": "bf93c4cc-6063-458e-afa4-b024f5c9abb6",
                "inputs": [
                    { "name": "turn", "type": "int", "value": 0 },
                    { "name": "pot", "type": "int", "value": 5 }
                ],
                "outputs": [
                    { "name": "game_over", "type": "boolean" },
                    { "name": "payoff", "type": "int" },
                    { "name": "new_pot", "type": "int" }
                ],
                "expected_cost": null,
                "expected_value": null
            }
        ]
    }

will be converted to the Lisp form,

    (:ACTIONS
     #((:ACTORS #("A") :URN "urn:centipede:take" :NAME "Take" :ID
        "d44cc237-9e09-4cb9-97aa-6f32831df844" :INPUTS
        #((:NAME "turn" :TYPE "int" :VALUE 0) (:NAME "pot" :TYPE "int" :VALUE 5))
        :OUTPUTS
        #((:NAME "game_over" :TYPE "boolean") (:NAME "payoff" :TYPE "int"))
        :EXPECTED-COST NULL :EXPECTED-VALUE (:PAYOFF 4))
       (:ACTORS #("A") :URN "urn:centipede:push" :NAME "Push" :ID
        "bf93c4cc-6063-458e-afa4-b024f5c9abb6" :INPUTS
        #((:NAME "turn" :TYPE "int" :VALUE 0) (:NAME "pot" :TYPE "int" :VALUE 5))
        :OUTPUTS
        #((:NAME "game_over" :TYPE "boolean") (:NAME "payoff" :TYPE "int")
          (:NAME "new_pot" :TYPE "int"))
        :EXPECTED-COST NULL :EXPECTED-VALUE NULL)))

When performing the inverse transformation on the value returned by `run-model` JSON object keys
are create by taking the print name of the Lisp keyword symbol, downcasing it, and replacing hyphens by underscores.
This corresponds to the convention I believe we have adopted in this project of always using snake_case for such keys
when they are multi-word. Note that if other conventions are used they will typically be corrected converted to Lisp
keywords on input, but to snake_case on output. If we change the convention it will be easy to change the output
form to some other convention, but it must be used uniformly.

If there is no `cl-user::run-model` function defined a stub version (`jh::default-run-model`) is called instead, which
simply returns all the supplied action IDs with equal probabilities to drive random behavior, the example output from the same document referred to above. It will also
write to the log file the Lisp representations of the incoming JSON and constant return value; this may be
useful for testing and/or understanding the Lisp format of the JSON when crafting the `run-model` function.

And this currently has only one query end point. When we want more it should not be difficult to add them.


##  Online version

There is also currently running a copy of this server on on `mneme.lan.cmu.edu` which may be useful for testing.
Currently this version does not have a real `cl-user::run-model` so uses the default, but we can easily update
it when a real `run-model` function is available. To use it simply point an HTTP client, sending the JSON
input using the POST method, at

    http://mneme.lan.cmu.edu:9899/decision


## Errors

If there is an error obtaining the POST data, assembling it into a UTF-8 string, or parsing that string as JSON,
an HTTP error code 400 will be returned. For other errors an HTTP error code 500 will be returned.
In either case there will also be a returned value, a JSON string containing a message describing the error.



