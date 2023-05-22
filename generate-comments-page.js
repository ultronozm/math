const axios = require('axios');
const fs = require('fs');

const owner = 'ultronozm';
const repo = 'math';
const numComments = 5;

const fetchRecentDiscussions = async () => {
  const response = await axios.get(`https://api.github.com/repos/${owner}/${repo}/discussions`, {
    headers: {
      Accept: 'application/vnd.github.squirrel-girl-preview+json',
      Authorization: `Bearer ${process.env.GITHUB_TOKEN}`
    }
  });

  const discussions = response.data.sort((a, b) => new Date(b.updated_at) - new Date(a.updated_at));

  return discussions.slice(0, numComments).map(discussion => ({
    title: discussion.title,
    url: discussion.html_url,
    lastCommentedBy: discussion.comments.nodes[0].user.login,
    lastCommentedAt: discussion.comments.nodes[0].updated_at,
    commentText: discussion.comments.nodes[0].body
  }));
};

fetchRecentDiscussions().then((discussions) => {
  fs.writeFileSync('recent-discussions.json', JSON.stringify(discussions, null, 2));
});
