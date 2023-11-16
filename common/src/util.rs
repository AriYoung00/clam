use std::{iter::IntoIterator, sync::mpsc::{Sender, SendError}};

pub trait SendAll<IterType, ErrorType> {
    fn send_all(&mut self, items: impl IntoIterator<Item = IterType>) -> ErrorType;
}

impl<T> SendAll<T, Result<(), SendError<T>>> for Sender<T> {
    fn send_all(&mut self, items: impl IntoIterator<Item = T>) -> Result<(), SendError<T>> {
        for item in items {
            self.send(item)?;
        }

        Ok(())
    }
}