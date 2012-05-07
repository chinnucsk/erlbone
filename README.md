#What it is
Following up on Addy Osmani's [Backbone tutorial](http://addyosmani.github.com/backbone-fundamentals/), I decided to put the Frontend part of one of his [boilerplates](https://github.com/addyosmani/backbone-boilerplates) onto an Erlang/Webmachine stack.

At the same time it serves me (possibly you also) as a boilerplate for Webmachine resources.

#How to use it
You should have [rebar](https://github.com/basho/rebar) on your $PATH. 

Check:
    rebar -V
    => rebar version: 2 date: 20120415_115608 ...more...
    erl
    => Erlang R14B04 (erts-5.8.5)  ...more ...

    git clone https://github.com/davidmathei/erlbone.git
    cd erlbone
    make
    make webstart

And visit http://localhost:9009

#Todos or shortcomings
* replace mnesia:dirty... calls by real transactions
* Cache headers, Modified Since and Etag handling is missing 
* No real persistence. Mnesia is now in-memory only.
* read Port, mnesia dir and persistence adjustable from configuration file
* write tests 

