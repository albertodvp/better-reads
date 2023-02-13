function prepare(formData) {
    const operation = formData.get('operation');
    const limit = formData.get('limit');
    const booksFile = formData.get('booksFile')
    const url = `booksOperation/${operation}?limit=${limit}`
    return {url, booksFile}
}

function post(url, file) {
    fetch(url, {
        method: "POST",
        headers: {'Content-Type': "application/octet-stream"},
        body: file
    })
        .then((res) => res.blob())
        .then((blob) => {
            let a = document.createElement("a");
            let blobURL = URL.createObjectURL(blob);
            a.download = "better-reads.csv";
            a.href = blobURL;
            document.body.appendChild(a);
            a.click();
            document.body.removeChild(a);
        });
}

function formSubmission(e) {
    e.preventDefault();
    console.log(e)
    let {url, booksFile} = prepare(new FormData(e.target));
    console.log(url);
    post(url, booksFile)
};
document.addEventListener('readystatechange', () => {
    if (document.readyState == 'complete') {
        document.getElementById('form').addEventListener('submit', formSubmission);
    }
});
