FROM erlang:21
ADD .leoronic_cookie /
ADD core/* core/
RUN cd core && erlc *.erl
CMD erl -sname leoronic -setcookie `cat .leoronic_cookie` -pa core -s leoronic