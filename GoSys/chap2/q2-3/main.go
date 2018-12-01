package main

import (
	"compress/gzip"
	"encoding/json"
	"io"
	"net/http"
	"os"
)

func handler(w http.ResponseWriter, r *http.Request)  {
	w.Header().Set("Content-Encoding", "gzip")
	w.Header().Set("Content-Type", "application/json")
	source := map[string]string {
		"Hello": "World",
	}
	writer := io.MultiWriter(gzip.NewWriter(os.Stdout), os.Stdout)
	encoder := json.NewEncoder(writer)
	encoder.Encode(source)
}

func main() {
	http.HandleFunc("/", handler)
	http.ListenAndServe(":8080", nil)
}
