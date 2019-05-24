FROM erlang:21
Add * /
ADD core/* core/
ADD ebin/* ebin/
RUN cd core && erlc +native *.erl
CMD erl -sname leoronic -setcookie `cat .leoronic_cookie` -s leoronic