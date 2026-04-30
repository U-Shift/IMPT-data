const main = function () {

    // .sidebar-title a
    const sidebar_title = document.querySelector(".sidebar-title a");
    if (sidebar_title) {
        sidebar_title.innerHTML = "<img src='images/logo.png' width='100%' height='auto' /><br/>Methodological Report<br/>";
    }

    // .quarto-title h1.title
    const page_title = document.querySelector(".quarto-title h1.title");
    if (page_title && page_title.textContent === "IMPT - Methodological Report") {
        page_title.textContent = "Methodological Report";
    }
}

document.addEventListener('DOMContentLoaded', main, false);