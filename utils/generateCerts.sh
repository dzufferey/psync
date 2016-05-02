#!/bin/bash
PSYNC_BASE="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/../"

function generateRoot() {
  mkdir -p root
  cd root
  echo "Generating root key and cert..."
  openssl genrsa -out ca.key.pem 4096 > /dev/null 2>&1
  openssl req -config $PSYNC_BASE/utils/cert_gen.cnf \
      -key ca.key.pem \
      -new -x509 -days 7300 -sha256 -extensions v3_ca \
      -out ca.cert.pem -batch
  cd ..
}

function generateSingle() {
  echo "Generating certificate #$1"
  cd ../root
  touch index.txt
  echo "01" > serial
  cd ../client
  openssl genrsa -out client.$1.key.pem 2048 > /dev/null 2>&1
  openssl req -config $PSYNC_BASE/utils/cert_gen.cnf \
      -key client.$1.key.pem -new -sha256 \
      -out client.$1.csr.pem -batch
  openssl ca -config $PSYNC_BASE/utils/cert_gen.cnf \
      -extensions generic_cert -days 7200 -notext -md sha256 \
      -keyfile ../root/ca.key.pem \
      -cert ../root/ca.cert.pem \
      -in client.$1.csr.pem \
      -out client.$1.cert.pem \
      -outdir . -batch > /dev/null 2>&1
  rm client.$1.csr.pem
  rm 01.pem
  cd ../root
  rm *.old *.attr index.txt serial
  cd ../client
}

function generateClients() {
  mkdir -p client
  cd client
  echo "Generating client certificates..."
  for i in $(seq $1); do
    generateSingle $i
  done
  cd ..
}

cd $PSYNC_BASE
if [ $# -eq 0 ]
  then
    echo "-- usage:    $0 <number of replicas>"
    echo "This script generates a root certificate and signed client certificates into the 'certs' directory in the root of this project. These can be imported by psync." | fmt
    exit
fi
mkdir -p certs
cd certs
generateRoot
generateClients $1
cd ..
