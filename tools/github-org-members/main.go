package main

import (
	"context"
	"fmt"
	"os"
	"time"

	log "github.com/sirupsen/logrus"

	"github.com/google/go-github/github"
	"github.com/urfave/cli/v2"
	"golang.org/x/oauth2"
)

func main() {
	log.SetOutput(os.Stdout)
	log.SetLevel(log.FatalLevel)

	app := &cli.App{
		Name:     "github-org-members",
		Compiled: time.Now(),
		Version:  "v0.0.0",
		HelpName: "github-org-members",
		Usage:    "retrieve a list of members in a GitHub organisation",
		Authors: []*cli.Author{
			{
				Name:  "Geoffrey Huntley",
				Email: "ghuntley@ghuntley.com",
			},
		},
		Flags: []cli.Flag{
			&cli.StringFlag{
				Name:     "token",
				Aliases:  []string{"t"},
				Usage:    "GitHub API token",
				EnvVars:  []string{"GITHUB_API_TOKEN"},
				Required: true,
			},
			&cli.StringFlag{
				Name:     "organisation",
				Aliases:  []string{"org", "o"},
				Usage:    "GitHub Organisation",
				Required: true,
			},
		},
		Action: func(c *cli.Context) error {
			var organisation = c.String("organisation")
			var token = c.String("token")

			ctx := context.Background()
			ts := oauth2.StaticTokenSource(
				&oauth2.Token{AccessToken: token},
			)
			tc := oauth2.NewClient(ctx, ts)
		
			client := github.NewClient(tc)
			opts := &github.ListMembersOptions{}
		
			for {
				organizations, response, err := client.Organizations.ListMembers(context.Background(), organisation, opts)
				if err != nil {
					return err
				}
		
				log.Trace("GitHub Rate Limit", response.Rate)
		
				log.Trace("Current Page: ", opts.Page)
				opts.Page = response.NextPage
				
				for i, organization := range organizations {
					i = i + 1
					fmt.Println(organization.GetLogin())
				}
        
        log.Trace("Next Page: ", response.NextPage)
				if response.NextPage == 0 {
					log.Trace("Final page!")
		
					break
				}

			}
		
			return nil
		},
	}

	err := app.Run(os.Args)
	if err != nil {
		log.Fatal(err)
	}
}
