#!/usr/bin/env python3

from http.server import HTTPServer, BaseHTTPRequestHandler

class Server(BaseHTTPRequestHandler):
    def do_GET(self):
        if self.path == '/':
            self.path = '/index.html'
        try:
            #Reading the file
            file_to_open = open(self.path[1:]).read()
            self.send_response(200)
        except:
            file_to_open = "File not found"
            self.send_response(404)

        self.end_headers()
        self.wfile.write(bytes(file_to_open, 'utf-8'))

if __name__=='__main__':
    PORT = 9000
    address = '0.0.0.0'
    httpd = HTTPServer((address, PORT), Server)

    print("Serving Http on", address, "port", PORT, "...")
    httpd.serve_forever()
