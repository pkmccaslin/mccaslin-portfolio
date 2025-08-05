// takes document object and applies this to all h2 elements within class .accordion
// each of these will be called the variable "header"
document.querySelectorAll('.accordion h2').forEach(header => {
    // header will have style cursor
    header.style.cursor = "pointer";
    // add click event listener
    header.addEventListener('click', function() {
        // define sibling as next element that's the same as the current one
        let sibling = header.nextElementSibling;
        // boolean of 
        let isOpen = sibling && sibling.style.display !== "none";
        // Hide all siblings until next h2
        while (sibling && sibling.tagName !== "H2") {
            sibling.style.display = isOpen ? "none" : "";
            sibling = sibling.nextElementSibling;
        }
    });
});