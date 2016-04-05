Phi Accrual Failure Detector
============================

`fd` is an implementation of [this](http://www.jaist.ac.jp/~defago/files/pdf/IS_RR_2004_010.pdf) paper which is published in 2004 and is implemented in Erlang. If you are interested to know more about Failure detectors in general and how they are classified please refer to this [page](http://www.cs.yale.edu/homes/aspnes/pinewiki/FailureDetectors.html).

### How to build the project
In order to build and run the project make sure you have Erlang/OTP 18 or newer is installed. In the root of the project run the following:
```
> erl -make
> erl -pa ebin/ -I include/ -sname node1
```
`node1` is just a name given to that Erlang node. Once inside the shell, you can start the application like this:
```
Erlang/OTP 18 [erts-7.2.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V7.2.1  (abort with ^G)
(node1@localhost)1> application:start(fd).
ok
```
And also stop the application like this:
```
(node1@localhost)1> application:stop(fd).
ok
```
For list of API you can call you can refer to [`fd.erl`](https://github.com/amir343/fd/blob/master/src/fd.erl) file.


License
-------

Copyright (C) 2016 [Amir Moulavi](http://amirmoulavi.com)

Distributed under the MIT License.