package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"net/http"
	"os"
	"time"

	"github.com/sirupsen/logrus"
	log "github.com/sirupsen/logrus"

	"github.com/urfave/cli/v2"
)

func main() {
	log.SetOutput(os.Stdout)
	log.SetLevel(log.TraceLevel)

	app := &cli.App{
		Name:     "orbit-cli",
		Compiled: time.Now(),
		Version:  "v0.0.0",
		HelpName: "orbit-cli",
		Usage:    "a command line interface for orbit.love",
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
				Usage:    "Orbit API token",
				EnvVars:  []string{"ORBIT_API_TOKEN"},
				Required: true,
			},
			&cli.StringFlag{
				Name:     "workspace",
				Aliases:  []string{"w"},
				Usage:    "Orbit API token",
				EnvVars:  []string{"ORBIT_WORKSPACE_ID"},
				Required: true,
			},
		},

		Commands: []*cli.Command{
			{
				Name:     "import",
				Usage:    "Import a user into Orbit",
				Category: "member",
				Subcommands: []*cli.Command{
					{
						Name:  "github",
						Usage: "from GitHub",
						Flags: []cli.Flag{
							&cli.StringFlag{Name: "username", Usage: "A GitHub username", Aliases: []string{"u"}, Required: true},
							&cli.StringFlag{Name: "tags", Usage: "Comma-separated string of tags to add", Aliases: []string{"t"}},
						},
						Action: func(c *cli.Context) error {

							var workspace = c.String("workspace")
							var token = c.String("token")
							var username = c.String("username")
							var tags = c.String("tags")

							log.WithFields(log.Fields{
								"user":      username,
								"workspace": workspace,
								"tags":      tags,
								"token":     token,
							}).Debug("CLI flags")

							if tags != "" {
								data := []byte(`{"member":{"github":"` + username + `", "tags": "` + tags + `"}}`)

								createOrUpdateMember(workspace, token, data)
							} else {
								data := []byte(`{"member":{"github":"` + username + `"}}`)

								createOrUpdateMember(workspace, token, data)
							}

							return nil
						},
					},
					{
						Name:  "linkedin",
						Usage: "from LinkedIn.",
						Flags: []cli.Flag{
							&cli.StringFlag{Name: "username", Usage: "A LinkedIn username, without the in/ or pub/", Aliases: []string{"u"}, Required: true},
							&cli.StringFlag{Name: "tags", Usage: "Comma-separated string of tags to add", Aliases: []string{"t"}},
						},
						Action: func(c *cli.Context) error {

							var workspace = c.String("workspace")
							var token = c.String("token")
							var username = c.String("username")
							var tags = c.String("tags")

							log.WithFields(log.Fields{
								"user":      username,
								"workspace": workspace,
								"tags":      tags,
								"token":     token,
							}).Debug("CLI flags")

							if tags != "" {
								data := []byte(`{"member":{"linkedin":"` + username + `", "tags": "` + tags + `"}}`)

								createOrUpdateMember(workspace, token, data)
							} else {
								data := []byte(`{"member":{"linkedin":"` + username + `"}}`)

								createOrUpdateMember(workspace, token, data)
							}

							return nil
						},
					},
					{
						Name:  "twitter",
						Usage: "from Twitter",
						Flags: []cli.Flag{
							&cli.StringFlag{Name: "username", Usage: "A Twitter username", Aliases: []string{"u"}, Required: true},
							&cli.StringFlag{Name: "tags", Usage: "Comma-separated string of tags to add", Aliases: []string{"t"}},
						},
						Action: func(c *cli.Context) error {

							var workspace = c.String("workspace")
							var token = c.String("token")
							var username = c.String("username")
							var tags = c.String("tags")

							log.WithFields(log.Fields{
								"user":      username,
								"workspace": workspace,
								"tags":      tags,
								"token":     token,
							}).Debug("CLI flags")

							if tags != "" {
								data := []byte(`{"member":{"twitter":"` + username + `", "tags": "` + tags + `"}}`)

								createOrUpdateMember(workspace, token, data)
							} else {
								data := []byte(`{"member":{"twitter":"` + username + `"}}`)

								createOrUpdateMember(workspace, token, data)
							}

							return nil
						},
					},
				},
			},
		},
	}

	err := app.Run(os.Args)
	if err != nil {
		log.Fatal(err)
	}
}

func createOrUpdateMember(workspace string, token string, data []byte) {
	url := fmt.Sprintf("https://app.orbit.love/api/v1/%s/members", workspace)

	req, err := http.NewRequest("POST", url, bytes.NewBuffer(data))
	if err != nil {
		log.Fatal("Error reading request. ", err)
	}

	req.Header.Set("Accept", "application/json")
	req.Header.Set("Content-Type", "application/json")

	req.Header.Set("Authorization", fmt.Sprintf(" Bearer %s", token))

	client := &http.Client{Timeout: time.Second * 10}

	log.WithFields(log.Fields{
		"body":    req.Body,
		"headers": req.Header,
		"url":     req.URL,
	}).Debug("Request")

	resp, err := client.Do(req)
	if err != nil {
		log.Fatal("Error reading response. ", err)
	}
	defer resp.Body.Close()

	log.WithFields(log.Fields{
		"status": resp.Status,
		"header": req.Header,
	}).Debug("Response")

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Fatal("Error reading body. ", err)
	}

	logrus.WithField("body", string(body[:])).Debug("Body")
}
