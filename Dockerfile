FROM erlang:21
Add * /
ADD leoronic/* leoronic/
RUN erlc +native leoronic/*.erl
CMD (erl +c true -name leoronic -setcookie `cat .leoronic_cookie` -run leoronic/leoronic)