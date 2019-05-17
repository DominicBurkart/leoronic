FROM erlang:21
Add * /
ADD leoronic/* leoronic/
ADD ebin/* ebin/
RUN cd leoronic && erlc +native +debug *.erl
CMD erl -name leoronic -setcookie `cat .leoronic_cookie` -s leoronic