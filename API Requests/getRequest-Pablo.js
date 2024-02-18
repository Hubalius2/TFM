import ads from './onlyAdID.json' assert { type: "json" };

// To write to file at the end
import fs from 'fs';

function delay(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

function getRandomInt(max) {
    return Math.floor(Math.random() * max);
}

const customUA = [
    'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.0.0 Safari/537.36',
    'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.0.0 Safari/537.36',
    'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 Safari/537.36',
    'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 Safari/537.36',
    'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 Safari/537.36',
    'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
    'Mozilla/5.0 (Macintosh; Intel Mac OS X 13_1) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15'
];

const getAd = (id) => {

    let userAgent = customUA[getRandomInt(customUA.length)];

    return fetch("https://ms-mt--api-web.spain.advgo.net/details/" + id, {
     method: "GET",
     headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json',
        'User-Agent': userAgent,
        'authority': 'ms-mt--api-web.spain.advgo.net',
        'sec-ch-ua': userAgent,
        'x-adevinta-channel': 'web-desktop',
        'x-schibsted-tenant': 'coches',
        'sec-ch-ua-mobile': '?0',
        'origin': 'https://www.coches.net',
        'sec-fetch-site': 'cross-site',
        'sec-fetch-mode': 'cors',
        'sec-fetch-dest': 'empty',
        'referer': 'https://www.coches.net/',
        'accept-language': 'accept-language',
        'Accept-Encoding': 'application/json;charset=UTF-8'
     },
   })
   .then(response => response.json())
   .then(data => {
    
        console.log(data);
        
        let carVersion = JSON.stringify(data);
        
        fs.writeFile('adJSONs/'+id +'.json', carVersion, 'utf8', function (err) {
            if (err) throw err;
            console.log('complete');
        });

   })
   .catch(error => console.error(error));
}


async function runPromises() {

    // for (let ad of ads) {

    for(let i = 12785; i < 25570; i++) {

        let carPrint =await getAd(ads[i].postID);

        await delay(700 + getRandomInt(300)); // .7s min - 1s max
    }
   
}

runPromises()

