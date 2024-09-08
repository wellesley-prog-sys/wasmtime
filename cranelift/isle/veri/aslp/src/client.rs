use reqwest::IntoUrl;
use serde::Deserialize;
use tracing::debug;

use crate::{ast::Block, parser};

pub struct Client {
    client: reqwest::blocking::Client,
    server_url: reqwest::Url,
}

impl Client {
    pub fn new<U: IntoUrl>(
        client: reqwest::blocking::Client,
        server_url: U,
    ) -> anyhow::Result<Self> {
        Ok(Self {
            client,
            server_url: server_url.into_url()?,
        })
    }

    pub fn opcode(&self, opcode: u32) -> anyhow::Result<Block> {
        // Model for response JSON data.
        #[derive(Deserialize, Debug)]
        struct Response {
            instruction: String,
            semantics: String,
        }

        // Issue GET request.
        let opcode_hex = format!("{opcode:#x}");
        let res: Response = self
            .client
            .get(self.server_url.clone())
            .query(&[("opcode", &opcode_hex)])
            .send()?
            .json()?;

        debug!(%res.semantics);

        // Ensure response instruction matches.
        if res.instruction != opcode_hex {
            anyhow::bail!("response opcode mismatch");
        }

        // Parse semantics.
        let block = parser::parse(&res.semantics)?;

        Ok(block)
    }
}
