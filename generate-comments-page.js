const axios = require('axios');
const fs = require('fs');

const owner = 'ultronozm';
const repo = 'math';

async function fetchRecentComments() {
    try {
	const response = await axios({
	    url: 'https://api.github.com/graphql',
	    method: 'post',
	    headers: {
		Authorization: `Bearer ${process.env.GITHUB_TOKEN}`,
	    },
	    data: {
		query: `
          query RecentDiscussions($owner: String!, $repo: String!) {
            repository(owner: $owner, name: $repo) {
              discussions(first: 10, orderBy: {field: UPDATED_AT, direction: DESC}) {
                nodes {
                  title
                  url
                  comments(last: 10) {
                    nodes {
                      id
                      author {
                        login
                      }
                      createdAt
                    }
                  }
                }
              }
            }
          }
        `,
		variables: { owner, repo },
	    },
	});

	if (response.data.errors) {
	    console.error('Error while fetching discussions:', response.data.errors);
	    return [];
	}
	
	return response.data.data.repository.discussions.nodes.map(discussion => ({
	    title: discussion.title,
	    url: discussion.url,
	    recentComments: discussion.comments.nodes.map(comment => ({
		id: comment.id,
		url: `${discussion.url}#discussioncomment-${comment.id}`,
		author: comment.author.login,
		createdAt: comment.createdAt,
	    })),
	}));
    } catch (err) {
	console.error('Error while executing GraphQL query:', err);
	return [];
    }
}

fetchRecentComments().then(comments => {
  fs.writeFileSync('recent-discussions.json', JSON.stringify(comments, null, 2));
}).catch(err => {
  console.error(err);
  process.exit(1);
});
