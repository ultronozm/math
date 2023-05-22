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
                  comments(last: 1) {
                    nodes {
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
      recentComment: discussion.comments.nodes[0] ? {
        author: discussion.comments.nodes[0].author.login,
        createdAt: discussion.comments.nodes[0].createdAt,
      } : null,
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



// const axios = require('axios');
// const fs = require('fs');

// // Replace with your repository's owner and name
// const owner = 'ultronozm';
// const repo = 'math';

// // GraphQL query to fetch recent discussions
// const query = `
//   query RecentDiscussions($owner: String!, $repo: String!) {
//     repository(owner: $owner, name: $repo) {
//       discussions(first: 10, orderBy: {field: UPDATED_AT, direction: DESC}) {
//         nodes {
//           title
//           url
//         }
//       }
//     }
//   }
// `;

// // Function to fetch recent discussions from the GitHub API
// async function fetchRecentDiscussions() {
//   const response = await axios({
//     url: 'https://api.github.com/graphql',
//     method: 'post',
//     headers: {
//       Authorization: `Bearer ${process.env.GITHUB_TOKEN}`,
//     },
//     data: {
//       query,
//       variables: { owner, repo },
//     },
//   });

//   if (response.data.errors) {
//     console.error(response.data.errors);
//     throw new Error('Failed to fetch recent discussions');
//   }

//   return response.data.data.repository.discussions.nodes;
// }

// // Function to generate an HTML page with a list of discussions
// function generateHtml(discussions) {
//   return `
//     <!DOCTYPE html>
//     <html>
//     <head>
//       <title>Recent Discussions</title>
//     </head>
//     <body>
//       <h1>Recent Discussions</h1>
//       <ul>
//         ${discussions.map(discussion => `
//           <li>
//             <a href="${discussion.url}">${discussion.title}</a>
//           </li>
//         `).join('')}
//       </ul>
//     </body>
//     </html>
//   `;
// }

// // Fetch recent discussions and generate an HTML page
// fetchRecentDiscussions().then(discussions => {
//   const html = generateHtml(discussions);
//   fs.writeFileSync('recent-discussions.html', html);
// }).catch(err => {
//   console.error(err);
//   process.exit(1);
// });

