main();

async function main() {
    const kalk = await import("kalk-rs");

    try {
        console.log(kalk.simple_eval("5+"));
    } catch(err) {
        console.log(err);
    }
}