# Jekyll Setup for Lisp in 2025

This repository is now configured as a Jekyll site that will automatically deploy to GitHub Pages when you push commits to the main branch.

## Automatic Deployment

The site will automatically build and deploy when you:
- Push commits to the `main` branch
- The GitHub Actions workflow (`.github/workflows/jekyll.yml`) handles the build and deployment

## Repository Settings

To enable GitHub Pages deployment, configure your repository:

1. Go to your repository Settings
2. Navigate to Pages (under "Code and automation")
3. Under "Build and deployment":
   - Source: Select "GitHub Actions"
4. Save the settings

The site will be available at: `https://cloudstreet-dev.github.io/Lisp-in-2025/`

## Local Development

To build and preview the site locally:

```bash
# Install dependencies (requires Ruby)
bundle install

# Serve the site locally
bundle exec jekyll serve

# The site will be available at http://localhost:4000/Lisp-in-2025/
```

## Site Structure

- `_config.yml` - Jekyll configuration
- `_layouts/` - Page templates (default, home)
- `_includes/` - Reusable components (navigation)
- `assets/css/` - Stylesheets
- `assets/js/` - JavaScript files
- Chapter files (*.md) - Book content with frontmatter

## Features

- **Responsive Design**: Mobile-friendly layout with sidebar navigation
- **Chapter Navigation**: Previous/Next links at the bottom of each chapter
- **Keyboard Navigation**: Use arrow keys to navigate between chapters
- **Code Highlighting**: Syntax highlighting for Lisp code examples
- **Copy Buttons**: Click to copy code snippets
- **SEO Optimized**: Meta tags and sitemap generation

## Customization

Edit `_config.yml` to customize:
- Site title and description
- Book metadata
- Chapter list and navigation
- Theme settings

## Testing Changes

After making changes:
1. Test locally with `bundle exec jekyll serve`
2. Commit and push to `main`
3. GitHub Actions will automatically build and deploy
4. Check the Actions tab to monitor deployment status
