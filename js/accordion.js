// select all of my resume-box which each has an id such as experience, education, or skills
const buttons = document.querySelectorAll('.resume-box');

// for each of those resume-box do the following
buttons.forEach(button => {
    button.addEventListener('click', () => {

        // buttons.forEach(box => {
        //     if (box.className !== 'hidden'){
        //         box.classList.add('hidden');
        //     }
        // });

        // define targetId as (for example) experience-hidden
        // this is the id of the content below
        const targetID = button.id + '-hidden';
        // targetElement is then retrieved from below
        const targetElement = document.getElementById(targetID);

        const allContentBoxes = document.querySelectorAll('.content')

        allContentBoxes.forEach(box => {
            if (box.id !== targetID && !box.classList.contains('hidden')) {
                box.classList.add('hidden'); // Add 'hidden' class to hide it
            }
        });

        if (targetElement) {


            const isNowHidden = targetElement.classList.toggle('hidden');

            // If the element just became visible (i.e., it was not hidden previously)
            // then scroll to it.
            if (!isNowHidden) {
                targetElement.scrollIntoView({ behavior: 'smooth', block: 'start' });
            }

        }


        // const hiddenContent = document.querySelectorAll('.resume-box');
        // hiddenContent.forEach(box => {
        //     if (box.id !== targetID && !box.classList.contains('hidden')) {
        //         box.classList.add('hidden')
        //     }
        // })
        // if (targetElement) {
        //     targetElement.classList.toggle('hidden')
        // }
    })
})

