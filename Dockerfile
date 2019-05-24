FROM erlang:21
Add * /
ADD leoronic/* leoronic/
ADD ebin/* ebin/
RUN cd leoronic && erlc +native *.erl
CMD erl -sname leoronic -setcookie `cat .leoronic_cookie` -s leoronic