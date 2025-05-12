## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE, message = FALSE-------------------------------------
library(smcryptoR)

## -----------------------------------------------------------------------------
msg <- charToRaw('abc')
sm3_hash(msg)

## -----------------------------------------------------------------------------
sm3_hash_string('abc')

## -----------------------------------------------------------------------------
## generate a keypair
keypair <- sm2_gen_keypair()
sk <- keypair$private_key
pk <- keypair$public_key
sk
pk

## -----------------------------------------------------------------------------
pk <- sm2_pk_from_sk(sk)
pk

## -----------------------------------------------------------------------------
id <- 'someone@company.com' |> charToRaw()
data <- 'abc' |> charToRaw()
sign <- sm2_sign(id, data, sk)
## return 1 or 0
sm2_verify(id, data, sign, pk)

## -----------------------------------------------------------------------------
## encrypt using public key
enc <- sm2_encrypt(data, pk)
## cipher text
enc
## decrypt using private key
dec <- sm2_decrypt(enc, sk)
## plain text
dec
## convert to character string
rawToChar(dec)

## -----------------------------------------------------------------------------
enc <- sm2_encrypt_base64(data, pk)
## cipher text as base64
enc
sm2_decrypt_base64(enc, sk) |> rawToChar()

## -----------------------------------------------------------------------------
enc <- sm2_encrypt_hex(data, pk)
## cipher text as hex
enc
sm2_decrypt_hex(enc, sk) |> rawToChar()

## -----------------------------------------------------------------------------
## Step 1
klen <- 16
id_a <- "a@company.com" |> charToRaw()
id_b <- "b@company.com" |> charToRaw()
private_key_a <- sm2_gen_keypair()$private_key
private_key_b <- sm2_gen_keypair()$private_key
step_1_a <- sm2_keyexchange_1ab(klen, id_a, private_key_a)
step_1_b <- sm2_keyexchange_1ab(klen, id_b, private_key_b)

## Step 2
step_2_a <- sm2_keyexchange_2a(id_a, private_key_a, step_1_a$private_key_r, step_1_b$data)
step_2_b <- sm2_keyexchange_2b(id_b, private_key_b, step_1_b$private_key_r, step_1_a$data)
step_2_a$k
step_2_b$k

## -----------------------------------------------------------------------------
## ecb mode
key <- '1234567812345678' |> charToRaw()
enc <- sm4_encrypt_ecb(data, key)
## cipher text
enc
## plain text
sm4_decrypt_ecb(enc, key) |> rawToChar()

## -----------------------------------------------------------------------------
iv <- '0000000000000000' |> charToRaw()
enc <- sm4_encrypt_cbc(data, key, iv)
## cipher text
enc
sm4_decrypt_cbc(enc, key, iv) |> rawToChar()

