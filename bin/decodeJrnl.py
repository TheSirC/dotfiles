import hashlib, Crypto.Cipher
key = hashlib.sha256("thelifesgood").digest()
with open("Journal") as f:
    cipher = f.read()
    crypto = AES.new(key, AES.MODE_CBC, iv = cipher[:16])
    plain = crypto.decrypt(cipher[16:])
    plain = plain.strip(plain[-1])
    plain = plain.decode("utf-8")
