import http from 'k6/http';
import { sleep } from 'k6';

export const options = {
  stages: [
    { duration: '30s', target: 5 },
    { duration: '30s', target: 10 },
    { duration: '30s', target: 20 },
    { duration: '30s', target: 30 },
  ],
};

function randomThinkTime() {
  return Math.max(1, Math.random() * Math.random() * 10);
}

export default function () {
  let res = http.get('https://huggingface.co/spaces/robertvidigal/CGD');
  if (res.status !== 200) console.log(`code=${res.status}`);
  sleep(randomThinkTime());
}
