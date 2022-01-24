FROM erlang

WORKDIR /app
COPY archtest .

RUN chmod u+x archtest
RUN ./archtest