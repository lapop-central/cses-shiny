import http from 'k6/http';
import { sleep } from 'k6';

export const options = {
  vus: 15,          // start here; increase later
  duration: '2m',   // longer run gives more realistic signal
};

function randomThinkTime() {
  // human-like think time: 1 to 10 seconds
  return Math.max(1, Math.random() * Math.random() * 10);
}

export default function () {
  // simulate page interaction
  let res = http.get('https://huggingface.co/spaces/robertvidigal/CGD');

  // log non-200s for diagnosis (optional)
  if (res.status !== 200) {
    console.log(`NON-200: ${res.status}`);
  }

  sleep(randomThinkTime());
}
