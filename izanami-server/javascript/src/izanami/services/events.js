
let callback = [];

let eventSource;

const IzanamiEvents = {
    start() {
        if (!!window.EventSource) {
            console.log('Initialising server sent event');
            eventSource = new EventSource(`${window.__contextPath}/api/events`);
            eventSource.addEventListener('open', e => {
                console.log('SSE opened');
            }, false);
            eventSource.addEventListener('error', e => {
                console.error('SSE error', e);
            }, false);
            eventSource.addEventListener('message', e => {
                console.log('New event', e);
                const data = JSON.parse(e.data);
                callback.forEach(cb => cb(data));
            }, false);
        }
    },
    stop() {
        if (eventSource) {
            eventSource.close();
        }
    }
};

export {IzanamiEvents};




export function addCallback(cb) {
  callback = [...callback, cb];
}

export function removeCallback(cb) {
  callback = callback.filter(c => c === cb);
}